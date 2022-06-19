/*
 *  Machine.scala
 *  (Tagfalter)
 *
 *  Copyright (c) 2022 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.tagfalter

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.{Random, RandomObj}
import de.sciss.numbers.Implicits.floatNumberWrapper
import de.sciss.proc.{TimeRef, Universe}
import de.sciss.synth.UGenSource.Vec
import de.sciss.tagfalter.Main.{ConfigAll, T, log}

import scala.concurrent.stm.Ref

object Machine {
  private[this] val sync        = new AnyRef
  private[this] var _instance   = Option.empty[Machine]

  def instance: Option[Machine] = sync.synchronized(_instance)

  def apply()(implicit tx: T, config: ConfigAll, universe: Universe[T]): Machine = {
    require (instance.isEmpty)
    implicit val random: Random[T] = RandomObj()
    val res = new Impl(random)
    sync.synchronized {
      _instance = Some(res)
    }
    res.start()
    res
  }

  private final class Impl(val random: Random[T])(implicit val universe: Universe[T], val config: ConfigAll)
    extends Machine {

    implicit def machine: Machine = this

    private val stageRef        = Ref[Stage](Stage.Empty)
    private val targetStageRef  = Ref[Stage](Stage.Empty)
    private val stagePosRef     = Ref[Vec[Float]](Vec.empty)
    private val runningRef      = Ref(Option.empty[Stage.Running])
    private val accelRecRef     = Ref(Option.empty[Accelerate.RecResult])
    private val accelRecTime    = Ref(0L)

    def start()(implicit tx: T): Unit = {
      targetStage_=(Stage.Crypsis)
    }

    override def accelerateRec(implicit tx: T): Option[Accelerate.RecResult] =
      accelRecRef()

    override def stage(implicit tx: T): Stage = stageRef()

    override def targetStage(implicit tx: T): Stage = targetStageRef()

    override def targetStage_=(value: Stage)(implicit tx: T): Unit = {
      // val oldTarget = targetStageRef.swap(value)
      targetStageRef() = value
      val sch = universe.scheduler
      // try to start next stage with a second delay
      sch.schedule(sch.time + TimeRef.SampleRate.toLong) { implicit tx =>
        tryLaunchTarget()
      }
    }

    private def tryLaunchTarget()(implicit tx: T): Unit = {
      if (runningRef().isDefined) return

      val st      = targetStageRef()
      stageRef()  = st
      val running = st.run()
      runningRef() = Some(running)
      val nextSt: Stage = st match {
        case Stage.Crypsis      => Stage.DetectSpace
        case Stage.DetectSpace  => Stage.SpaceTimbre
        case Stage.SpaceTimbre  =>
          val timeNow = universe.scheduler.time
          val timeRec = accelRecTime()
          if (accelRecRef().isDefined && timeRec != 0L && (timeNow - timeRec) / TimeRef.SampleRate >= config.accelRecTime) {
            Stage.Accelerate
          } else {
            Stage.Crypsis
          }
        case Stage.Accelerate   => Stage.Crypsis
        case _                  => Stage.Empty
      }
      targetStageRef() = nextSt

      log.info(s"Starting $st")
      running.start()
    }

    override def spacePos(implicit tx: T): Vec[Float] = stagePosRef()

    override def spacePos_=(value: Vec[Float])(implicit tx: T): Unit = {
      stagePosRef() = value
      random.setSeed(value.sum.toLong)
      // we launch accel-rec after the first space info becomes available
      if (accelRecRef().isEmpty) {
        val mn          = value.min
        val mx          = value.max
        val mean        = value.sum / value.size
        val skew        = mean.linLin(mn, mx, 0.0f, 1.0f)
        val accelFactor = skew.linExp(0.0f, 1.0f, config.accelMinFactor, config.accelMaxFactor)
        val accelBufDur = config.accelRecTime / accelFactor
        log.info(f"New accel factor $accelFactor%1.1f; buf dur $accelBufDur%1.1f")
        val rc = Accelerate.recWith(accelFactor = accelFactor, accelBufDur = accelBufDur)
        accelRecRef()   = Some(rc)
        accelRecTime()  = universe.scheduler.time
      }
    }

    override def released(st: Stage)(implicit tx: T): Unit = {
      if (stage == st) {
        log.info(s"Released $st")
        runningRef() = None
        tryLaunchTarget()
      }
    }
  }
}
trait Machine {
  implicit val universe: Universe[T]

  implicit def config: ConfigAll

  def spacePos(implicit tx: T): Vec[Float]
  def spacePos_=(value: Vec[Float])(implicit tx: T): Unit

  def accelerateRec(implicit tx: T): Option[Accelerate.RecResult]

  def random: Random[T]

  def stage(implicit tx: T): Stage

  def targetStage(implicit tx: T): Stage

  def targetStage_=(value: Stage)(implicit tx: T): Unit

  def released(stage: Stage)(implicit tx: T): Unit
}
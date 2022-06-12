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

import de.sciss.lucre.{InTxnRandom, Random, RandomObj}
import de.sciss.lucre.Txn.peer
import de.sciss.proc.Universe
import de.sciss.synth.UGenSource.Vec
import de.sciss.tagfalter.Main.{ConfigAll, T, log}

import scala.concurrent.stm.{InTxn, Ref}

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
    private val releasedRef     = Ref(true)

    def start()(implicit tx: T): Unit =
      targetStage_=(Stage.CrypsisStage)

    override def stage(implicit tx: T): Stage = stageRef()

    override def targetStage(implicit tx: T): Stage = targetStageRef()

    override def targetStage_=(value: Stage)(implicit tx: T): Unit = {
/**/      val oldTarget = targetStageRef.swap(value)
      targetStageRef() = value
      tryLaunchTarget()
    }

    private def tryLaunchTarget()(implicit tx: T): Unit = {
      if (!releasedRef()) return

      val st = targetStageRef.swap(Stage.Empty)
      stageRef() = st
      log.info(s"Starting $st")
      st.start()
    }

    override def spacePos(implicit tx: T): Vec[Float] = stagePosRef()

    override def spacePos_=(value: Vec[Float])(implicit tx: T): Unit =
      stagePosRef() = value

    override def released(st: Stage)(implicit tx: T): Unit = {
      if (stage == st) {
        log.info(s"Released $st")
        releasedRef() = true
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

  def random: Random[T]

  def stage(implicit tx: T): Stage

  def targetStage(implicit tx: T): Stage

  def targetStage_=(value: Stage)(implicit tx: T): Unit

  def released(stage: Stage)(implicit tx: T): Unit
}
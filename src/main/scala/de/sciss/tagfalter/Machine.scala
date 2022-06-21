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
import de.sciss.tagfalter.Biphase.MessageSpaceId
import de.sciss.tagfalter.Main.{ConfigAll, T, log}

import scala.concurrent.stm.Ref

object Machine {
  private[this] val sync        = new AnyRef
  private[this] var _instance   = Option.empty[Machine]

  private val INTER_DELAY_MIN = 1.0f
  private val INTER_DELAY_MAX = 2.0f

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

  private val HEX = "0123456789ABCDEF"

  private final class Impl(val random: Random[T])(implicit val universe: Universe[T], val config: ConfigAll)
    extends Machine {

    implicit def machine: Machine = this

    private val stageRef        = Ref[Stage](Stage.Empty)
    private val targetStageRef  = Ref[Stage](Stage.Empty)
    private val stagePosRef     = Ref[Vec[Float]](Vec.empty)
    private val runningRef      = Ref(Option.empty[Stage.Running])
    private val accelRecRef     = Ref(Option.empty[Accelerate.RecResult])
    private val accelRecTime    = Ref(0L)
    private val detectSpaceTime = Ref(0L)
    private val stageSilNext    = Ref[Stage](Stage.Empty)
    private val rcvSpaceId      = Ref(0L)
    private val timeSpaceId     = Ref(0L)

    // 6 bytes in `MessageSpaceId` times 10 bits per byte, one second extra time out
    private val TimeOutSpaceId  = ((config.bitPeriod/1000 * (10 * MessageSpaceId.NumBytes) + 1f) * TimeRef.SampleRate).toLong

    def start()(implicit tx: T): Unit = {
      startBiphaseRcv()
      targetStage_=(Stage.Crypsis)
    }

    private def received(m: Biphase.Message)(implicit tx: T): Unit = {
      log.info(s"Received $m")
    }

    private def startBiphaseRcv()(implicit tx: T): Unit = {
      /*val rcvGlobal =*/ Biphase.receive(f1 = config.biphaseF1a, f2 = config.biphaseF2a) { implicit tx => byte =>
        // if (byte == Biphase.CMD_HOLD_ON)
        val hexHi = (byte >> 4) & 0x0F
        val hexLo =  byte       & 0x0F
        log.debug(s"Received global comm 0x${HEX(hexHi)}${HEX(hexLo)}")
        val now = universe.scheduler.time
        if (byte == Biphase.CMD_HOLD_ON) {
          rcvSpaceId()  = 0L
          received(Biphase.MessageHoldOn)

        } else if (byte == Biphase.CMD_SPACE_ID) {
          rcvSpaceId()  = 1L
          timeSpaceId() = now

        } else {
          val rcvSpaceIdOld = rcvSpaceId()
          val byteCntOld = (rcvSpaceIdOld & 0xFF).toInt
          if (byteCntOld > 0 && byteCntOld < MessageSpaceId.NumBytes) {
            if (now - timeSpaceId() < TimeOutSpaceId) {
              val payloadOld    = rcvSpaceIdOld & 0xFFFFFFFFFFFF00L
              val payloadNew    = (payloadOld | (byte & 0xFF)) << 8
              val byteCntNew    = byteCntOld + 1
              val rcvSpaceIdNew = payloadNew | byteCntNew
              rcvSpaceId()      = rcvSpaceIdNew
              if (byteCntNew == MessageSpaceId.NumBytes) {
                println(s"payloadNew = $payloadNew")
                val nodeId  =  ((payloadNew >> (5 * 8)) & 0xFF).toInt
                val f1      = (((payloadNew >> (4 * 8)) & 0x7F).toInt << 7) |
                               ((payloadNew >> (3 * 8)) & 0x7F).toInt
                val f2      = (((payloadNew >> (2 * 8)) & 0x7F).toInt << 7) |
                               ((payloadNew >> (1 * 8)) & 0x7F).toInt
                val mId = Biphase.MessageSpaceId(nodeId = nodeId, f1 = f1, f2 = f2)
                received(mId)
              }
            } else {
              log.debug(s"Global comm time out")
              rcvSpaceId() = 0L
            }
          }
        }
      }
    }

    override def commFreq(implicit tx: T): Vec[(Float, Float)] =
      Vec((config.biphaseF1a, config.biphaseF2a))

    override def accelerateRec(implicit tx: T): Option[Accelerate.RecResult] =
      accelRecRef()

    override def stage(implicit tx: T): Stage = stageRef()

    override def targetStage(implicit tx: T): Stage = targetStageRef()

    override def targetStage_=(value: Stage)(implicit tx: T): Unit = {
      // val oldTarget = targetStageRef.swap(value)
      targetStageRef() = value
      val sch = universe.scheduler
      // try to start next stage with a second delay
      val INTER_DELAY = random.nextFloat().linLin(0f, 1f, INTER_DELAY_MIN, INTER_DELAY_MAX)
      sch.schedule(sch.time + (TimeRef.SampleRate * INTER_DELAY).toLong) { implicit tx =>
        tryLaunchTarget()
      }
    }

    private def tryLaunchTarget()(implicit tx: T): Unit = {
      if (runningRef().isDefined) return

      val st        = targetStageRef()
      stageRef()    = st
      val running   = st.run()
      runningRef()  = Some(running)
      val timeNow   = universe.scheduler.time
      val nextSt0: Stage = st match {
        case Stage.Crypsis =>
          val timeDetectOld = detectSpaceTime()
          if (timeDetectOld == 0L || (timeNow - timeDetectOld) / TimeRef.SampleRate >= config.detectSpacePeriod) {
            detectSpaceTime() = timeNow
            Stage.DetectSpace
          } else {
            log.info("(skip detect-space this time)")
            Stage.SpaceTimbre
          }

        case Stage.DetectSpace => Stage.SpaceTimbre

        case Stage.SpaceTimbre =>
          val timeRec = accelRecTime()
          // XXX TODO: warning, `accelRecRef` may no longer be the same after the current stage
          // finishes, once we add another process that occasionally frees the ref!
          if (accelRecRef().isDefined && timeRec != 0L && (timeNow - timeRec) / TimeRef.SampleRate >= config.accelRecTime) {
            Stage.Accelerate
          } else {
            Stage.Crypsis
          }

        case Stage.Accelerate   => Stage.Crypsis
        case Stage.Silence      => stageSilNext()
        case _                  => Stage.Crypsis // Stage.Empty
      }

      val nextSil = st != Stage.Silence && random.nextFloat() < config.silenceProb
      val nextSt  = if (!nextSil) nextSt0 else {
        stageSilNext() = nextSt0
        Stage.Silence
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

  /** Currently known spatial positions in cm. Empty if unknown. */
  def spacePos(implicit tx: T): Vec[Float]
  def spacePos_=(value: Vec[Float])(implicit tx: T): Unit

  def accelerateRec(implicit tx: T): Option[Accelerate.RecResult]

  def random: Random[T]

  def stage(implicit tx: T): Stage

  def targetStage(implicit tx: T): Stage

  def targetStage_=(value: Stage)(implicit tx: T): Unit

  def released(stage: Stage)(implicit tx: T): Unit

  /** Currently known communication frequencies in Hz,
    * including the global ones; thus always returns at
    * least one tuple.
    */
  def commFreq(implicit tx: T): Vec[(Float, Float)]
}
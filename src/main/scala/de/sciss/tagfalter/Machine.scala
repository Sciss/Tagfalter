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
import de.sciss.numbers.Implicits._
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

  private case class JoyReceive(nodeId: Int, rcv: Biphase.Receive)

  private final class Impl(val random: Random[T])(implicit val universe: Universe[T], val config: ConfigAll)
    extends Machine {

    implicit def machine: Machine = this

    private val stageRef        = Ref[Stage](Stage.Empty)
    private val targetStageRef  = Ref[Stage](Stage.Empty)
    private val stagePosRef     = Ref[Vec[Float]](Vec.empty)
    private val runningRef      = Ref[Option[Stage.Running]](None)
    private val accelRecRef     = Ref[Option[Accelerate.RecResult]](None)
    private val accelRecTime    = Ref(0L)
    private val detectSpaceTime = Ref(0L)
    private val stageSaveNext   = Ref[Stage](Stage.Empty)
    private val rcvSpaceId      = Ref(0L)
    private val timeSpaceId     = Ref(0L)
    private val commFreqSeqRef  = Ref[Vec[Float]](Vec.empty)
    private val joyBlockTime    = Ref(0L)   // no joy response until this point in time has been reached
    private val joyRcvRef       = Ref[Vec[JoyReceive]](Vec.empty)
    private val joyResponse     = Ref(false)
    private val holdOn          = Ref(false)
    private val holdBlockTime   = Ref(0L)   // no hold response until this point in time has been reached

    // 6 bytes in `MessageSpaceId` times 10 bits per byte, one second extra time out
    private val TimeOutSpaceId    = ((config.bitPeriod/1000 * (10 * MessageSpaceId.NumBytes) + 1f) * TimeRef.SampleRate).toLong
    private val DetectSpacePeriod = config.detectSpacePeriod * 1.5.pow((config.nodeId % 32)/32.0)

    def start()(implicit tx: T): Unit = {
      startBiphaseRcv()
      targetStage_=(Stage.Crypsis)
    }

    private def received(m: Biphase.Message)(implicit tx: T): Unit = {
      log.info(s"Received $m")

      val sch = universe.scheduler
      val now = sch.time
      m match {
        case Biphase.MessageHoldOn =>
          if (now > holdBlockTime() && !holdOn()) {
            holdOn() = true
            runningRef().foreach(_.release())
            val dlySec    = random.nextFloat().linLin(0f, 1f, 10f, 20f)
            log.info(f"Hold on for $dlySec%1.1fs")
            val dlyFr     = (dlySec * TimeRef.SampleRate).toLong
            sch.schedule(now + dlyFr) { implicit tx =>
              holdOn() = false
              tryLaunchTarget()
            }
          }

        case m @ Biphase.MessageSpaceId(nodeId, _ /*f1*/, _ /*f2*/) =>
          val joyRcvOld = joyRcvRef()
          val rlsIdx0   = joyRcvOld.indexWhere(_.nodeId == nodeId)
          // release the receiver for the existing node, or otherwise release the oldest
          // so we never have more than two receivers
          val rlsIdx    = if (rlsIdx0 >= 0) rlsIdx0 else if (joyRcvOld.size > 1) 1 else -1
          val joyRcvMed = if (rlsIdx < 0) joyRcvOld else {
            val joyRls = joyRcvOld(rlsIdx)
            joyRls.rcv.dispose()
            joyRcvOld.patch(rlsIdx, Nil, 1)
          }
          val joyAtk    = mkJoyReceiver(m)
          val joyRcvNew = joyAtk +: joyRcvMed
          joyRcvRef()   = joyRcvNew

        case Biphase.MessageJoy(_ /*nodeId*/) =>
          val commFreqOpt = thisCommFreq
          if (now > joyBlockTime() && !joyResponse() && commFreqOpt.isDefined) {
            val dlySec    = random.nextFloat().linLin(0f, 1f, 1f, 4f)
            val dlyFr     = (dlySec * TimeRef.SampleRate).toLong
            val timeResp  = now + dlyFr
            joyBlockTime() = timeResp + (TimeRef.SampleRate * 10).toLong
            joyResponse() = true
            runningRef().foreach(_.release())
            sch.schedule(timeResp) { implicit tx =>
              log.info("Sending joy response")
              Biphase.send(m.encode, commFreqOpt.get) { implicit tx =>
                joyResponse() = false
                val dlySec    = random.nextFloat().linLin(0f, 1f, 2f, 4f)
                log.info(f"Hold on for $dlySec%1.1fs")
                val dlyFr     = (dlySec * TimeRef.SampleRate).toLong
                sch.schedule(sch.time + dlyFr) { implicit tx =>
                  tryLaunchTarget()
                }
              }
            }
          }

        case _ => ()  // `Message` is sealed, does not occur
      }
    }

    private def mkJoyReceiver(m: Biphase.MessageSpaceId)(implicit tx: T): JoyReceive = {
      val rcvAtk = Biphase.receive(f1 = m.f1.toFloat, f2 = m.f2.toFloat) { implicit tx => byte =>
        val hexHi = (byte >> 4) & 0x0F
        val hexLo =  byte       & 0x0F
        log.debug(s"Received node comm 0x${HEX(hexHi)}${HEX(hexLo)}")
        if (byte == Biphase.CMD_JOY) {
          // Note: we don't actually wait for the nodeId byte
          received(Biphase.MessageJoy(0))
        }
      }
      JoyReceive(m.nodeId, rcvAtk)
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
                // println(s"payloadNew = $payloadNew")
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

    override def allCommFreq(implicit tx: T): Vec[Float] = {
      val commFreqSeq = commFreqSeqRef()
      val xs = Vec(config.biphaseF1a, config.biphaseF2a)
      if (commFreqSeq.isEmpty) xs else xs :+ commFreqSeq(0) :+ commFreqSeq(1)
    }

    override def otherCommFreq(implicit tx: T): Vec[Float] =
      Vec(config.biphaseF1a, config.biphaseF2a)

    override def thisCommFreq(implicit tx: T): Option[Biphase.Freq] = {
      val commFreqSeq = commFreqSeqRef()
      if (commFreqSeq.isEmpty) None else {
        val f = Biphase.Freq(
          f1a = commFreqSeq(0), f1b = commFreqSeq(2), f2a = commFreqSeq(1), f2b = commFreqSeq(3)
        )
        Some(f)
      }
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
      val INTER_DELAY = random.nextFloat().linLin(0f, 1f, INTER_DELAY_MIN, INTER_DELAY_MAX)
      sch.schedule(sch.time + (TimeRef.SampleRate * INTER_DELAY).toLong) { implicit tx =>
        tryLaunchTarget()
      }
    }

    private def tryLaunchTarget()(implicit tx: T): Unit = {
      if (runningRef().isDefined || joyResponse() || holdOn()) {
        log.info(s"launch skipped. running? ${runningRef().isDefined}; joy? ${joyResponse()}; hold? ${holdOn()}")
        return
      }

      val st        = targetStageRef()
      stageRef()    = st
      val running   = st.run()
      runningRef()  = Some(running)
      val timeNow   = universe.scheduler.time
      val nextSt0: Stage = st match {
        case Stage.Crypsis =>
          val timeDetectOld = detectSpaceTime()
          if (timeDetectOld == 0L || (timeNow - timeDetectOld) / TimeRef.SampleRate >= DetectSpacePeriod) {
            detectSpaceTime() = timeNow
            Stage.DetectSpace
          } else {
            log.info("(skip detect-space this time)")
            Stage.SpaceTimbre
          }

        case Stage.DetectSpace =>
          holdBlockTime() = timeNow + (TimeRef.SampleRate * 10).toLong
          Stage.SpaceTimbre

        case Stage.SpaceTimbre =>
          val timeRec = accelRecTime()
          // XXX TODO: warning, `accelRecRef` may no longer be the same after the current stage
          // finishes, once we add another process that occasionally frees the ref!
          if (accelRecRef().isDefined && timeRec != 0L && (timeNow - timeRec) / TimeRef.SampleRate >= config.accelRecTime) {
            Stage.Accelerate
          } else {
            Stage.Crypsis
          }

        case Stage.Accelerate => Stage.Crypsis
        case Stage.Silence    => stageSaveNext()

        case Stage.Joy  =>
          joyBlockTime() = timeNow + (TimeRef.SampleRate * 10).toLong
          stageSaveNext()

        case _ => Stage.Crypsis // Stage.Empty
      }

      val nextSil = st != Stage.Silence && random.nextFloat() < config.silenceProb
      val nextJoy = !nextSil && st != Stage.Joy && random.nextFloat() < config.joyProb
      val nextSt  = if (nextSil) {
        stageSaveNext() = nextSt0
        Stage.Silence
      } else if (nextJoy) {
        stageSaveNext() = nextSt0
        Stage.Joy
      } else {
        nextSt0
      }
      targetStageRef() = nextSt

      log.info(s"Starting $st")
      running.start()
    }

    override def spacePos(implicit tx: T): Vec[Float] = stagePosRef()

    private def shuffleN(xs: Vec[Float], n: Int)(implicit tx: T): Vec[Float] = {
      val xsSize = xs.size
      val buf = if (xsSize >= n) {
        xs.toArray
      } else if (xsSize == 0) {
        Array.fill(n)(1f)
      } else {
        Array.tabulate(n) { i =>
          if (i < xsSize) xs(i) else xs.last
        }
      }

      def swap(i1: Int, i2: Int): Unit = {
        val tmp = buf(i1)
        buf(i1) = buf(i2)
        buf(i2) = tmp
      }

      var m = buf.length
      while (m >= 2) {
        val k = random.nextInt(m)
        m -= 1
        swap(m, k)
      }

      val res = Vector.newBuilder[Float]
      res.sizeHint(n)
      var i = 0
      while (i < n) {
        res += buf(i)
        i += 1
      }
      res.result()
    }

    override def spacePos_=(value: Vec[Float])(implicit tx: T): Unit = {
      stagePosRef() = value
      random.setSeed(value.sum.toLong)

      // calculate comm freq based on this
      val commPosSeq: Vec[Float] = {
        val sh = shuffleN(value, 4).sorted
        val d1 = sh(1) - sh(0)
        val d2 = sh(2) - sh(1)
        val d3 = sh(3) - sh(2)
        if (d1 <= d2 && d1 <= d3) sh
        else if (d2 <= d1 && d2 <= d3) {
          Vector(sh(1), sh(2), sh(3), sh(0))
        } else {
          Vector(sh(2), sh(3), sh(0), sh(1))
        }
      }
      val skipFreq    = otherCommFreq /*.flatMap(tup => tup._1 ::  tup._2 :: Nil)*/.sorted
      val commFreqSeq = commPosSeq.map { cm =>
        import config.{commMaxFreq, commMinFreq, spaceMaxCm, spaceMinCm}
        val cmClip  = cm.clip(spaceMinCm, spaceMaxCm)
        val f0      = cmClip.linLin(spaceMinCm, spaceMaxCm, commMinFreq, commMaxFreq)
        skipFreq.foldLeft(f0) { (f1, fBlock) =>
          // XXX TODO: should we use relative frequencies, such as `fBlock * 0.99` ?
          //  I think Goertzel is on a linear spectrum, so probably fine like this
          val fBlockLo = fBlock - 100f
          val fBlockHi = fBlock + 100f
          if (f1 >= fBlockLo && f1 <= fBlockHi) fBlockHi else f1
        }
      }
      log.info(s"This comm freq $commFreqSeq")
      commFreqSeqRef() = commFreqSeq

//      val f1a = p1a.linLin(spaceMinCm, spaceMaxCm, idMinFreq, idMaxFreq)

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
    * least two elements (one tuple f1a, f2a).
    */
  def allCommFreq(implicit tx: T): Vec[Float]

  /** Currently known communication frequencies in Hz,
   * including the global ones, but not the own individual frequencies.
   */
  def otherCommFreq(implicit tx: T): Vec[Float]

  def thisCommFreq(implicit tx: T): Option[Biphase.Freq]
}
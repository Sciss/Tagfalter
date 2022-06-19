/*
 *  CrypsisStage.scala
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

import de.sciss.lucre.Disposable
import de.sciss.lucre.Txn.peer
import de.sciss.numbers.Implicits._
import de.sciss.proc.TimeRef
import de.sciss.tagfalter.Main.{T, log}

import scala.concurrent.stm.Ref

class CrypsisStage extends Stage.Running {
  private val crypRef   = Ref(Option.empty[Crypsis.Result])
  private val rcvRef    = Ref(Disposable.empty[T])
  private val tokenRef  = Ref(-1)

  override def stage: Stage = Stage.Crypsis

  override def start()(implicit tx: T, machine: Machine): Unit = {
    import machine.{config, random, universe}
    val spacePos  = machine.spacePos
    val modFreq   = if (spacePos.isEmpty) config.crypModFreq else {
      val sz: Int = spacePos.size.clip(config.minSpacePos, config.maxSpacePos)
      sz.linLin(config.minSpacePos, config.maxSpacePos, config.crypModMinFreq, config.crypModMaxFreq)
    }
    val cryp      = Crypsis.applyWith(modFreq = modFreq)
    crypRef()     = Some(cryp)
    /*val lCryp: Disposable[T] =*/ cryp.runner.reactNow { implicit tx => state =>
      if (state.idle) {
        // lCryp.dispose()
        if (crypRef.swap(None).contains(cryp)) {
          cryp.runner.dispose()
          machine.released(stage)
        }
      }
    }

    val sch         = universe.scheduler
    val schTimeRef  = Ref(0L)

    def schedule(minDur: Double, maxDur: Double)(implicit tx: T): Unit = {
      val dlySec  = random.nextDouble().linLin(0.0, 1.0, minDur, maxDur)
      log.info(f"Crypsis duration in seconds: $dlySec%1.1f")
      val dlyFr   = (dlySec * TimeRef.SampleRate).toLong
      val schTime = sch.time + dlyFr
      val token = sch.schedule(schTime) { implicit tx =>
        release()
      }
      schTimeRef() = schTime
      sch.cancel(tokenRef.swap(token))
    }

    schedule(minDur = config.initCrypMinDur, maxDur = config.initCrypMaxDur)

    val rcv = Biphase.receive() { implicit tx => _ /*byte*/ =>
      // if (byte == Biphase.CMD_HOLD_ON)
      log.info("Received global communication")
      val dlyGlobSec  = 10.0
      val dlyGlobFr   = (dlyGlobSec * TimeRef.SampleRate).toLong
      val timeMin     = sch.time + dlyGlobFr
      val schTime     = schTimeRef()
      if (schTime < timeMin) {
        log.info("Have to reschedule crypsis")
        // important to schedule further than `dlyGlobSec` because transmission could call
        // `consume` again now.
        val durGlobSec = 5.0
        schedule(minDur = dlyGlobSec + durGlobSec, maxDur = dlyGlobSec * 2 + durGlobSec)
      }
    }
    rcvRef() = rcv
  }

  override def pause ()(implicit tx: T, machine: Machine): Unit = () // XXX TODO
  override def resume()(implicit tx: T, machine: Machine): Unit = () // XXX TODO

  override def release()(implicit tx: T, machine: Machine): Unit = {
    crypRef().foreach { cryp =>
      log.debug("Releasing crypsis (and biphase rcv)")
      cryp.release()
    }
    rcvRef.swap(Disposable.empty).dispose()
    val sch = machine.universe.scheduler
    sch.cancel(tokenRef.swap(-1))
  }
}

/*
 *  Stage.scala
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
import de.sciss.numbers.Implicits.doubleNumberWrapper
import de.sciss.proc.TimeRef
import de.sciss.tagfalter.Main.{T, log}

import scala.concurrent.stm.Ref

object Stage {
  case object Empty extends Stage {
    final val id = 0

    override def start()(implicit tx: T, machine: Machine): Unit = ()

    def pause ()(implicit tx: T, machine: Machine): Unit = ()
    def resume()(implicit tx: T, machine: Machine): Unit = ()

    override def release()(implicit tx: T, machine: Machine): Unit = {
      import machine.universe.scheduler
      scheduler.schedule(scheduler.time + TimeRef.SampleRate.toLong) { implicit tx =>
        machine.released(this)
      }
    }
  }
  case object CrypsisStage extends Stage { stage =>
    final val id = 1

    override def start()(implicit tx: T, machine: Machine): Unit = {
      import machine.{universe, config, random}
      val cryp = Crypsis()
      /*val lCryp: Disposable[T] =*/ cryp.runner.reactNow { implicit tx => state =>
        if (state.idle) {
          // lCryp.dispose()
          cryp.runner.dispose()
          machine.released(stage)
        }
      }

      val sch         = universe.scheduler
      val tokenRef    = Ref(-1)
      val schTimeRef  = Ref(0L)
      val rcvRef      = Ref(Disposable.empty[T])

      def schedule(minDur: Double, maxDur: Double)(implicit tx: T): Unit = {
        val dlySec  = random.nextDouble().linLin(0.0, 1.0, minDur, maxDur)
        log.info((f"Crypsis duration in seconds: $dlySec%1.1f"))
        val dlyFr   = (dlySec * TimeRef.SampleRate).toLong
        val schTime = sch.time + dlyFr
        val token = sch.schedule(schTime) { implicit tx =>
          log.debug("Releasing crypsis (and biphase rcv)")
          cryp.release()
          rcvRef().dispose()
        }
        schTimeRef() = schTime
        sch.cancel(tokenRef.swap(token))
      }

      schedule(minDur = config.initCrypMinDur, maxDur = config.initCrypMaxDur)

      val rcv = Biphase.receive() { implicit tx => byte =>
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

    def pause ()(implicit tx: T, machine: Machine): Unit = ()
    def resume()(implicit tx: T, machine: Machine): Unit = ()

    override def release()(implicit tx: T, machine: Machine): Unit = {
      import machine.universe.scheduler
      scheduler.schedule(scheduler.time + TimeRef.SampleRate.toLong) { implicit tx =>
        machine.released(this)
      }
    }
  }
  case object AccelerateStage extends Stage {
    final val id = 2

    override def start()(implicit tx: T, machine: Machine): Unit = ()

    def pause ()(implicit tx: T, machine: Machine): Unit = ()
    def resume()(implicit tx: T, machine: Machine): Unit = ()

    override def release()(implicit tx: T, machine: Machine): Unit = {
      import machine.universe.scheduler
      scheduler.schedule(scheduler.time + TimeRef.SampleRate.toLong) { implicit tx =>
        machine.released(this)
      }
    }
  }
}
sealed trait Stage {
  def id: Int

  def start   ()(implicit tx: T, machine: Machine): Unit
  def release ()(implicit tx: T, machine: Machine): Unit
  def pause   ()(implicit tx: T, machine: Machine): Unit
  def resume  ()(implicit tx: T, machine: Machine): Unit
}

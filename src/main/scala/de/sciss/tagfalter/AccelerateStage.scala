/*
 *  AccelerateStage.scala
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
import de.sciss.numbers.Implicits.doubleNumberWrapper
import de.sciss.proc.TimeRef
import de.sciss.tagfalter.Main.{T, log}

import scala.concurrent.stm.Ref

class AccelerateStage extends Stage.Running {
  private val accRef    = Ref(Option.empty[Accelerate.PlayResult])
  private val tokenRef  = Ref(-1)

  override def stage: Stage = Stage.Accelerate

  override def start()(implicit tx: T, machine: Machine): Unit = {
    import machine.{config, random, universe}
    val rc    = machine.accelerateRec.getOrElse(sys.error("No acceleration recorder!"))
    val amp0  = config.accelSigAmp
    val amp   = if (config.isEisenerz) amp0 * 0.5f else if (config.isLoudCard) amp0 * 0.32f else amp0
    val acc   = Accelerate.play(rc, amp = amp)
    accRef() = Some(acc)
    /*val lCryp: Disposable[T] =*/ acc.runner.reactNow { implicit tx => state =>
      if (state.idle) {
        // lCryp.dispose()
        if (accRef.swap(None).contains(acc)) {
          acc.runner.dispose()
          machine.released(stage)
        }
      }
    }

    val sch         = universe.scheduler
    val schTimeRef  = Ref(0L)

    def schedule(minDur: Double, maxDur: Double)(implicit tx: T): Unit = {
      val dlySec  = random.nextDouble().linLin(0.0, 1.0, minDur, maxDur)
      log.info(f"Accelerate duration in seconds: $dlySec%1.1f")
      val dlyFr   = (dlySec * TimeRef.SampleRate).toLong
      val schTime = sch.time + dlyFr
      val token = sch.schedule(schTime) { implicit tx =>
        release()
      }
      schTimeRef() = schTime
      sch.cancel(tokenRef.swap(token))
    }

    schedule(minDur = config.initAccMinDur, maxDur = config.initAccMaxDur)
  }

  override def pause ()(implicit tx: T, machine: Machine): Unit = () // XXX TODO
  override def resume()(implicit tx: T, machine: Machine): Unit = () // XXX TODO

  override def release()(implicit tx: T, machine: Machine): Unit = {
    accRef().foreach { acc =>
      log.debug("Releasing accel-play")
      acc.release()
    }
//    rcvRef.swap(Disposable.empty).dispose()
    val sch = machine.universe.scheduler
    sch.cancel(tokenRef.swap(-1))
  }
}

/*
 *  SilenceStage.scala
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
import de.sciss.proc.{SoundProcesses, TimeRef}
import de.sciss.tagfalter.Main.{T, log}

import scala.concurrent.stm.Ref

class SilenceStage extends Stage.Running {
  private val synRef    = Ref(Disposable.empty[T])
  private val tokenRef  = Ref(-1)

  override def stage: Stage = Stage.Silence

  override def start()(implicit tx: T, machine: Machine): Unit = {
    import machine.{config, random, universe}
    val syn = Silence.applyWith(amp = config.silenceAmp.dbAmp, freq = config.silenceFreq)
    synRef.swap(syn).dispose()

    syn.onEndTxn { implicit tx =>
      tx.afterCommit {
        import universe.cursor
        SoundProcesses.step[T]("silence-done") { implicit tx =>
          machine.released(stage)
        }
      }
    }

    val sch     = universe.scheduler
    val durSec  = random.nextFloat().linLin(0f, 1f, config.silenceMinDur, config.silenceMaxDur)
    log.info(f"Silence duration $durSec%1.1f s")
    val durFr   = (TimeRef.SampleRate * durSec).toLong
    val token   = sch.schedule(sch.time + durFr) { implicit tx =>
      release()
    }
    val oldToken = tokenRef.swap(token)
    sch.cancel(oldToken)
  }

  override def pause ()(implicit tx: T, machine: Machine): Unit = () // XXX TODO
  override def resume()(implicit tx: T, machine: Machine): Unit = () // XXX TODO

  override def release()(implicit tx: T, machine: Machine): Unit = {
    synRef.swap(Disposable.empty).dispose()
    val sch = machine.universe.scheduler
    sch.cancel(tokenRef.swap(-1))
  }
}

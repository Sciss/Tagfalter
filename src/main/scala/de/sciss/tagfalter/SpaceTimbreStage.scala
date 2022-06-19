/*
 *  SpaceTimbreStage.scala
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

class SpaceTimbreStage extends Stage.Running {
  override def stage: Stage = Stage.SpaceTimbre

  private val timbreRef = Ref(Option.empty[SpaceTimbre.Result])
  private val tokenRef  = Ref(-1)

  override def start()(implicit tx: T, machine: Machine): Unit = {
    import machine.{config, random, universe}
    val timbre = SpaceTimbre(machine.spacePos)
    timbre.runner.reactNow { implicit tx => state =>
      if (state.idle) {
        if (timbreRef.swap(None).contains(timbre)) {
          timbre.runner.dispose()
          machine.released(stage)
        }
      }
    }
    timbreRef() = Some(timbre)

    val sch     = universe.scheduler
    val dlySec  = random.nextDouble().linLin(0.0, 1.0, config.spaceMinDur, config.spaceMaxDur)
    log.info(f"SpaceTimbre duration in seconds: $dlySec%1.1f")
    val dlyFr   = (dlySec * TimeRef.SampleRate).toLong
    val token   = sch.schedule(sch.time + dlyFr) { implicit tx =>
      release()
    }
    sch.cancel(tokenRef.swap(token))
  }

  override def release()(implicit tx: T, machine: Machine): Unit = {
    timbreRef().foreach { timbre =>
      log.debug("Releasing space-timbre")
      timbre.release()
    }
    val sch = machine.universe.scheduler
    sch.cancel(tokenRef.swap(-1))
  }

  override def pause  ()(implicit tx: T, machine: Machine): Unit = () // XXX TODO
  override def resume ()(implicit tx: T, machine: Machine): Unit = () // XXX TODO
}

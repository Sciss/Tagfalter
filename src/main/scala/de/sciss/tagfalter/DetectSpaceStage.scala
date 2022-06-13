/*
 *  DetectSpaceStage.scala
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

import de.sciss.proc.TimeRef
import de.sciss.tagfalter.Main.{T, log}

class DetectSpaceStage extends Stage.Running {
  override def stage: Stage = Stage.DetectSpace

  override def start()(implicit tx: T, machine: Machine): Unit = {
    import machine.{config, universe}
    Biphase.send(Array(Biphase.CMD_HOLD_ON)) { implicit tx =>
      log.info("Biphase send finished.")
      // implicit val cfgDetect = DetectSpace.ConfigImpl()
      DetectSpace() { implicit tx => resSpace =>
        log.info("DetectSpace finished.")
        // log.info(resSpace.posCm.mkString("Pos [cm]: ", ", ", ""))
//        spaceTimbre(resSpace.posCm)
        machine.spacePos_=(resSpace.posCm)
        release()
      }
    }
  }

  override def release()(implicit tx: T, machine: Machine): Unit = {
    import machine.universe.scheduler
    scheduler.schedule(scheduler.time + TimeRef.SampleRate.toLong) { implicit tx =>
      machine.released(stage)
    }
  }

  override def pause  ()(implicit tx: T, machine: Machine): Unit = ()
  override def resume ()(implicit tx: T, machine: Machine): Unit = ()
}

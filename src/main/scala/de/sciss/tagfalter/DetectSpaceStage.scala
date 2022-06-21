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

import de.sciss.numbers.Implicits._
import de.sciss.proc.TimeRef
import de.sciss.tagfalter.Biphase.{MessageHoldOn, MessageSpaceId}
import de.sciss.tagfalter.Main.{T, log}

class DetectSpaceStage extends Stage.Running {
  override def stage: Stage = Stage.DetectSpace

  private final val INIT_DELAY_MIN = 9      // seconds
  private final val INIT_DELAY_MAX = 11     // seconds
  private final val INTER_DELAY_MIN = 0.6f  // seconds
  private final val INTER_DELAY_MAX = 1.4f  // seconds

  override def start()(implicit tx: T, machine: Machine): Unit = {
    import machine.{config, universe, random}
    Biphase.send(MessageHoldOn.encode) { implicit tx =>
      log.info("Biphase HOLD send finished.")
      // implicit val cfgDetect = DetectSpace.ConfigImpl()
      val sch  = universe.scheduler
      val INIT_DELAY = random.nextFloat().linLin(0f, 1f, INIT_DELAY_MIN, INIT_DELAY_MAX)
      sch.schedule(sch.time + (TimeRef.SampleRate * INIT_DELAY).toLong) { implicit tx =>
        DetectSpace() { implicit tx => resSpace =>
          log.info("DetectSpace finished.")
          // log.info(resSpace.posCm.mkString("Pos [cm]: ", ", ", ""))
          //        spaceTimbre(resSpace.posCm)
          machine.spacePos_=(resSpace.posCm)
          val thisFreq = machine.thisCommFreq.get
          val f1  = thisFreq.f1a.toInt
          val f2  = thisFreq.f2a.toInt
          val mId = MessageSpaceId(nodeId = config.nodeId, f1 = f1, f2 = f2)
          Biphase.send(mId.encode) { implicit tx =>
            release()
          }
        }
      }
    }
  }

  override def release()(implicit tx: T, machine: Machine): Unit = {
    import machine.universe.scheduler
    import machine.random

    val INTER_DELAY = random.nextFloat().linLin(0f, 1f, INTER_DELAY_MIN, INTER_DELAY_MAX)
    scheduler.schedule(scheduler.time + (TimeRef.SampleRate * INTER_DELAY).toLong) { implicit tx =>
      machine.released(stage)
    }
  }

  override def pause  ()(implicit tx: T, machine: Machine): Unit = () // XXX TODO
  override def resume ()(implicit tx: T, machine: Machine): Unit = () // XXX TODO
}

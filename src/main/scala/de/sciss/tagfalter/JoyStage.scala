/*
 *  JoyStage.scala
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
import de.sciss.tagfalter.Main.T

class JoyStage extends Stage.Running {
  override def stage: Stage = Stage.Joy

  override def start()(implicit tx: T, machine: Machine): Unit = {
    import machine.{config, universe}
    val m = Biphase.MessageJoy(config.nodeId)
    machine.thisCommFreq match {
      case Some(freq) =>
        Biphase.send(m.encode, freq = freq, ampDb = config.encAmpComm) { implicit tx =>
          import machine.random
          val sch     = machine.universe.scheduler
          val dlySec  = random.nextFloat().linLin(0f, 1f, 7f, 15f)
          val dlyFr   = (dlySec * TimeRef.SampleRate).toLong
          sch.schedule(sch.time + dlyFr) { implicit tx =>
            machine.released(stage)
          }
        }

      case None =>
        machine.released(stage)
    }
  }

  override def pause  ()(implicit tx: T, machine: Machine): Unit = ()
  override def resume ()(implicit tx: T, machine: Machine): Unit = ()
  override def release()(implicit tx: T, machine: Machine): Unit = ()
}

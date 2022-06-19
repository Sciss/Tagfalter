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

import de.sciss.proc.TimeRef
import de.sciss.tagfalter.Main.T

object Stage {
  case object Empty extends Stage with Stage.Running {
    final val id = 0

    override def stage: Stage = this

    override def run(): Stage.Running = this

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

  case object Crypsis extends Stage {
    final val id = 1

    override final def run(): Stage.Running = new CrypsisStage
  }

  case object DetectSpace extends Stage {
    final val id = 2

    override final def run(): Stage.Running = new DetectSpaceStage
  }

  case object SpaceTimbre extends Stage {
    final val id = 3

    override final def run(): Stage.Running = new SpaceTimbreStage
  }

  case object Accelerate extends Stage  {
    final val id = 4

    override def run(): Stage.Running = new AccelerateStage
  }

  case object Silence extends Stage {
    final val id = 5

    override def run(): Stage.Running = new SilenceStage
  }

  trait Running {
    def stage: Stage

    def start   ()(implicit tx: T, machine: Machine): Unit
    def release ()(implicit tx: T, machine: Machine): Unit
    def pause   ()(implicit tx: T, machine: Machine): Unit
    def resume  ()(implicit tx: T, machine: Machine): Unit
  }
}
sealed trait Stage {
  def id: Int

  def run(): Stage.Running
}

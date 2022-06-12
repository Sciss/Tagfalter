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
  object Empty extends Stage {
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
  object Crypsis extends Stage {
    final val id = 1

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

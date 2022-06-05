/*
 *  Main.scala
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

import de.sciss.lucre.{DoubleVector, Workspace}
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.graph.{Ex, Var}
import de.sciss.lucre.expr.{Context, IExprAsRunnerMap}
import de.sciss.lucre.synth.{InMemory, Server}
import de.sciss.proc.{AuralSystem, Proc, Runner, SoundProcesses, Universe}
import de.sciss.synth.{Client, SynthGraph}

object Main {
  type T = InMemory.Txn

  final val SR = 48000

  case class Config()

  def main(args: Array[String]): Unit = {
    implicit val config: Config = Config()
    run()
  }

  def run()(implicit config: Config): Unit = {
    val as      = AuralSystem(global = true)
    val sCfg    = Server.Config()
    sCfg.inputBusChannels   = 1
    sCfg.outputBusChannels  = 1
    sCfg.deviceName         = Some("Tagfalter")
    val cCfg    = Client.Config()
    implicit val system: InMemory = InMemory()
    system.step { implicit tx =>
      as.react { tx => {
        case AuralSystem.Running(_) =>
          tx.afterCommit {
            SoundProcesses.step[T]("detect-space") { implicit tx =>
              implicit val ws: Workspace[T] = Workspace.Implicits.dummy[T]
              implicit val u: Universe[T] = Universe[T]()
              detectSpace()
            }
          }

        case _ => ()
      }}
      as.start(sCfg, cCfg)
    }
  }

  def detectSpace()(implicit tx: T, universe: Universe[T]): Unit = {
    val p = Proc[T]()
    p.graph() = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, _}
      // val sweep     = DiskIn.ar("in")
      val sweepFreq = Line.ar(100.0, 16000.0, 2)
      val sweep     = SinOsc.ar(sweepFreq)
      sweepFreq.poll(4, "sweep-freq")
      val gainIn    = 0.2 // "gain-in" .kr(0.2)
      val gainOut   = 4.0 // "gain-out".kr(4.0)
      val outChan   = 0 // "out-ch"    .kr(0)
      val durTotal  = 3.0 // "total-dur-sec".kr(3)
      val sigOut    = sweep * gainIn
      PhysicalOut.ar(outChan, sigOut)
      val sigIn     = PhysicalIn.ar * gainOut
      //DiskOut.ar("out", sigIn)
      val framesTotal = durTotal * SR
      val buf = Buffer.Empty(framesTotal)
      val r = RecordBuf.ar(sigIn, buf, loop = 0)
      val recDone = Done.kr(r)
      //StopSelf(Done.kr(elapsed))
      val getDone = Action.GetBuf(recDone, "out", buf)
      DoneSelf(getDone)
    }

    val vrSweep = DoubleVector.newVar[T](Vector.empty)
    p.attr.put("out", vrSweep)
    val r = Runner(p)
//    import universe.{cursor, workspace}
//    implicit val undo: UndoManager[T] = UndoManager.dummy[T]
//    implicit val ctx: Context[T] = Context[T](selfH = Some(tx.newHandle(p)))
//    val vrSweep = Var[Seq[Double]]()
//    val vrSweepEntry: Ex[(String, Seq[Double])] = ("out", vrSweep: Ex[Seq[Double]])
//    val vrSweepEx = vrSweep.expand[T]
//    val vrSweepEntryEx = vrSweepEntry.expand[T]
//    import ctx.targets
//    val map = new IExprAsRunnerMap[T](vrSweepEntryEx :: Nil, tx)
//    r.prepare(map)
    r.react { implicit tx => {
      case Runner.Done =>
        println("Doningsdorfer")
//        val sweep = vrSweepEx.value
        val sweep = vrSweep.value
        println(sweep.mkString(", "))
        sys.exit()

      case state =>
        println(s"(state $state)")
    }}
    println("Running sweep rec")
    r.run()
  }
}

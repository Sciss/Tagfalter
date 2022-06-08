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
import de.sciss.lucre.synth.{InMemory, Server}
import de.sciss.proc.{AuralSystem, Proc, Runner, SoundProcesses, Universe}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.{Client, SynthGraph}
import de.sciss.tagfalter.DetectSpace.{Config, detectSpace}

import scala.collection.Seq
import scala.util.control.NonFatal

object Main {
  type T = InMemory.Txn

  final val SR = 48000

  case class ConfigImpl() extends Config

  trait Config {

  }

  def main(args: Array[String]): Unit = {
    // DetectSpace.main(args)
    implicit val config: Config = ConfigImpl()
    run()
  }

  def run()(implicit config: Config): Unit = {
    boot { implicit tx => implicit universe => s =>
      implicit val cfgDetect: DetectSpace.Config = DetectSpace.ConfigImpl()
      DetectSpace(){ implicit tx => cmMean =>
        spaceTimbre(cmMean)
      }
    }
  }

  def spaceTimbre(vec: Vec[Float])(implicit tx: T, universe: Universe[T]): Unit = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      val space     = "space".kr(1.0) // Seq(1.0, 2.0))
      //space.poll(0, "space")
      val spaceLo   = Reduce.min(space)
      val spaceHi   = Reduce.max(space)
      spaceLo.poll(0, "space-lo")
      spaceHi.poll(0, "space-hi")
      val spaceMin  =    60.0 // 10.0
      val spaceMax  = 12000.0 // 2000.0
      val freqMin   = 150.0 // 250.0 // 150.0
      val freqMax   = 15000.0
      val freqSeq   = space.clip(spaceMin, spaceMax)
        .linExp(spaceMin, spaceMax, freqMin, freqMax)
      //val freqSeq   = space.clip(spaceMin, spaceMax)
      //  .linLin(spaceMin, spaceMax, freqMin, freqMax)
      val oscSeq    = SinOsc.ar(freqSeq)
      val oscSum    = Mix.Mono(oscSeq) / NumChannels(oscSeq)
      PhysicalOut.ar(0, oscSum * "amp".kr(0.1))
    }

    val p = Proc[T]()
    p.graph() = g
    val pAttr = p.attr
    pAttr.put("space", DoubleVector.newConst[T](vec.map(_.toDouble)))
    val r = Runner(p)
    r.run()
  }

  def printInfo(): Unit = {
    import BuildInfo._
    println(s"$name v$version, built $builtAtString")
  }

  def boot(done: T => Universe[T] => Server => Unit) = {
    val as      = AuralSystem(global = true)
    val sCfg    = Server.Config()
    sCfg.inputBusChannels   = 1
    sCfg.outputBusChannels  = 1
    sCfg.deviceName         = Some("Tagfalter")
    val cCfg    = Client.Config()
    implicit val system: InMemory = InMemory()
    system.step { implicit tx =>
      as.react { tx => {
        case AuralSystem.Running(s) =>
          tx.afterCommit {
            //            s.peer.dumpOSC()
            SoundProcesses.step[T]("booted") { implicit tx =>
              implicit val ws: Workspace[T] = Workspace.Implicits.dummy[T]
              implicit val u: Universe[T] = Universe[T]()
              done(tx)(u)(s)
            }
          }

        case _ => ()
      }}
      as.start(sCfg, cCfg)
    }
  }

  def macAddress(ifc: String = "wlan0"): Array[Byte] = {
    import scala.sys.process._
    try {
      val macAddress  = Seq("cat", s"/sys/class/net/$ifc/address").!!.trim
      val sq = macAddress.split(':')
      sq.map { s =>
        Integer.valueOf(s, 16).toByte
      }
    } catch {
      case _: Exception =>
        new Array[Byte](6)
    }
  }
}

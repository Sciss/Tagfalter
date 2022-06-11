/*
 *  MicRec.scala
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

import de.sciss.lucre.synth.{Buffer, Server, Synth}
import de.sciss.proc.Universe
import de.sciss.synth.{SynthGraph, freeSelf}
import de.sciss.tagfalter.Main.T
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.File

object MicRec {

  case class ConfigImpl(
                         debug  : Boolean = false,
                         amp    : Float   = 20.0f, // 4.0f,
                         dur    : Float   = 10.0f,
                         file   : File    = new File("output.aif")
                       ) extends Config

  trait Config {
    def debug   : Boolean
    def amp     : Float
    def dur     : Float
    def file    : File
  }


  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      printedName = "Tagfalter - MicRec"
      private val default = ConfigImpl()

      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )
      val amp: Opt[Float] = opt(default = Some(default.amp),
        descr = s"Microphone boost, linear (default: ${default.amp}).",
      )
      val dur: Opt[Float] = opt(default = Some(default.dur),
        descr = s"Duration in seconds (default: ${default.dur}).",
      )
      val file: Opt[File] = opt(required = true,
        descr = "Output file (AIFF)",
      )

      verify()
      implicit val config: Config = ConfigImpl(
        debug     = debug(),
        amp       = amp(),
        dur       = dur(),
        file      = file(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    Main.boot { implicit tx => implicit universe => s =>
      apply(s)
    }
  }

  def apply(s: Server)(implicit tx: T, config: Config, universe: Universe[T]): Unit = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.proc.graph.{DiskOut => _}
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
//      import de.sciss.synth.proc.graph.Ops.stringToControl
      val in0 = PhysicalIn.ar
      val in  = in0 * "amp".kr(1.0)
      val dur = "dur".kr(10.0)
      val buf = "buf".kr
      DiskOut.ar(buf, in)
      Line.kr(dur = dur, doneAction = freeSelf)
    }

    val buf = Buffer.diskOut(s)(config.file.getPath)

    val syn = Synth.play(g, Some("rec"))(s.defaultGroup, args = Seq(
      "amp" -> config.amp,
      "dur" -> config.dur,
      "buf" -> buf.id
    ))

    syn.onEndTxn { implicit tx =>
      buf.dispose()
      tx.afterCommit {
        println("Done.")
        sys.exit()
      }
    }
  }
}

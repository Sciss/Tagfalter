/*
 *  Silence.scala
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
import de.sciss.lucre.synth.Synth
import de.sciss.numbers.Implicits._
import de.sciss.proc.{Runner, Universe}
import de.sciss.synth.SynthGraph
import de.sciss.tagfalter.Main.T
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object Silence {

  case class ConfigImpl(
                         silenceAmp     : Float   = -30f,
                         silenceFreq    : Float   = 34f,
                       ) extends Config

  trait Config {
    /** Decibels */
    def silenceAmp        : Float
    def silenceFreq       : Float
  }

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      printedName = "Tagfalter - Silence"
      private val default = ConfigImpl()

      val silenceAmp: Opt[Float] = opt(default = Some(default.silenceAmp),
        descr = s"Silent click amp, in decibels (default: ${default.silenceAmp}).",
      )
      val silenceFreq: Opt[Float] = opt(default = Some(default.silenceFreq),
        descr = s"Silent click frequency, in Hz (default: ${default.silenceFreq}).",
      )

      verify()
      implicit val config: Config = ConfigImpl(
        silenceAmp        = silenceAmp(),
        silenceFreq       = silenceFreq(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    Main.boot { implicit tx => implicit universe => _ /*s*/ =>
      apply(/*s*/)
      ()
    }
  }

  type Result = Disposable[T]

  def apply(/*s: Server*/)(implicit tx: T, config: Config, universe: Universe[T]): Result = {
    applyWith(amp = config.silenceAmp.dbAmp, freq = config.silenceFreq)
  }

  def applyWith(amp: Float, freq: Float)(implicit t: T, universe: Universe[T]): Synth = {
    val s = universe.auralContext.get.server
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.proc.graph.{DiskOut => _}
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      // not so silent ;)
      val dust0 = Dust2.ar("freq".kr(0.2f))
      val dust  = BPZ2.ar(dust0)
      val sig   = dust * "amp".kr(1.0f)
      PhysicalOut.ar(0, sig)
    }

    val syn = Synth.play(g, Some("rec"))(s.defaultGroup, args = Seq(
      "amp"  -> amp,
      "freq" -> freq,
    ))

    syn
  }
}

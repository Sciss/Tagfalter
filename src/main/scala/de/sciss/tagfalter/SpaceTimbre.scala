/*
 *  SpaceTimbre.scala
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

import de.sciss.log.Level
import de.sciss.lucre.{BooleanObj, DoubleObj, DoubleVector}
import de.sciss.numbers.Implicits.{doubleNumberWrapper, floatNumberWrapper}
import de.sciss.proc.{Proc, Runner, TimeRef, Universe}
import de.sciss.synth.SynthGraph
import de.sciss.synth.UGenSource.Vec
import de.sciss.tagfalter.Main.{T, log}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object SpaceTimbre {

  case class ConfigImpl(
                         dur : Float = 30f,
                         numPos        : Int     = 5,
                         debug         : Boolean = false,
                         spaceMinCm    : Float   =    60.0f, // 10.0
                         spaceMaxCm    : Float   = 12000.0f, // 2000.0
                         spaceMinFreq  : Float   =   150.0f,
                         spaceMaxFreq  : Float   = 18000.0f,
                         spaceAmp      : Float   = -6.0f, // decibels

                       ) extends Config

  trait Config {
    def debug       : Boolean
    def spaceMinCm  : Float
    def spaceMaxCm  : Float
    def spaceMinFreq: Float
    def spaceMaxFreq: Float
    def spaceAmp    : Float
  }

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {
      printedName = "Tagfalter - SpaceTimbre"
      private val default = ConfigImpl()

      val numPos: Opt[Int] = opt(default = Some(default.numPos),
        descr = s"Number of positions to test (default: ${default.numPos}).",
        validate = _ > 0
      )
      val dur: Opt[Float] = opt(default = Some(default.dur),
        descr = s"Test duration in seconds (default: ${default.dur}).",
      )
      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )
      val spaceMinCm: Opt[Float] = opt(default = Some(default.spaceMinCm),
        descr = s"Minimum assumed spatial position in cm (default: ${default.spaceMinCm}).",
      )
      val spaceMaxCm: Opt[Float] = opt(default = Some(default.spaceMaxCm),
        descr = s"Maximum assumed spatial position in cm (default: ${default.spaceMaxCm}).",
      )
      val spaceMinFreq: Opt[Float] = opt(default = Some(default.spaceMinFreq),
        descr = s"Minimum assumed space-timbre frequency in Hz (default: ${default.spaceMinFreq}).",
      )
      val spaceMaxFreq: Opt[Float] = opt(default = Some(default.spaceMaxFreq),
        descr = s"Maximum assumed space-timbre frequency in Hz (default: ${default.spaceMaxFreq}).",
      )
      val spaceAmp: Opt[Float] = opt(default = Some(default.spaceAmp),
        descr = s"Space-timbre amplitude, in decibels (default: ${default.spaceAmp}).",
      )

      verify()
      implicit val config: ConfigImpl = ConfigImpl(
        dur           = dur(),
        numPos        = numPos(),
        debug         = debug(),
        spaceMinCm    = spaceMinCm(),
        spaceMaxCm    = spaceMaxCm(),
        spaceMinFreq  = spaceMinFreq(),
        spaceMaxFreq  = spaceMaxFreq(),
        spaceAmp      = spaceAmp(),
      )
    }
    import p.config
    run()
  }

  trait Result {
    def runner: Runner[T]

    def release()(implicit tx: T): Unit
  }

  def run()(implicit config: ConfigImpl): Unit = {
    if (config.debug) log.level = Level.Debug
    Main.boot { implicit tx => implicit universe => _ /*s*/ =>
      val vec   = Vec.fill(config.numPos)(math.random().toFloat
        .linLin(0.0f, 1.0f, config.spaceMinCm, config.spaceMaxCm))
      val vecS  = vec.sorted
      val res   = apply(vecS)
      res.runner.reactNow { implicit tx => state =>
        if (state.idle) {
          log.info("Timbre released")
          tx.afterCommit {
            sys.exit()
          }
        }
      }

      // println(config.dur)
      if (config.dur > 0f) {
        val sch     = universe.scheduler
        val durFr   = (config.dur * TimeRef.SampleRate).toLong
        sch.schedule(sch.time + durFr) { implicit tx =>
          log.info("Release timbre")
          res.release()
        }
      }
    }
  }

  def apply(vec: Vec[Float])(implicit tx: T, config: Config, universe: Universe[T]): Result = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.DoneSelf
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      val freqSeq = "freq-seq".kr(150.0) // Seq(1.0, 2.0))
      //space.poll(0, "space")

      // XXX TODO: is this good, referring to the number of channels of `vec`?
      val oscAmpSeq = Vec.tabulate(vec.size) { i =>
        LFNoise1.kr(i.linLin(0, vec.size - 1, 0.11, 0.23)).abs
      }
      val oscSeq    = SinOsc.ar(freqSeq) * oscAmpSeq
      val oscSum    = Mix.Mono(oscSeq) / NumChannels(oscSeq)
      val amp       = "amp".kr(0.1)
      val gate      = "gate".kr(1)
      val env       = EnvGen.kr(Env.asr(attack = 5f, level = amp, release = 10f), gate = gate)
      DoneSelf(Done.kr(env))
      PhysicalOut.ar(0, oscSum * env)
    }

    val p = Proc[T]()
    p.graph() = g
    val pAttr = p.attr

    val spaceSeq = vec match {
      case Vec()        => Vec(120.0f, 5000.0f)
      case Vec(single)  => Vec(single, single * 1.5f)
      case _            => vec
    }
    val spaceLo   = spaceSeq.min
    val spaceHi   = spaceSeq.max
    log.debug(f"space-lo = $spaceLo%1.1f")
    log.debug(f"space-hi = $spaceHi%1.1f")
    // XXX TODO: we should skip the global communication frequencies!
    import config.{spaceMaxCm, spaceMaxFreq, spaceMinCm, spaceMinFreq}
    val freqSeq = spaceSeq.map(_.toDouble.clip(spaceMinCm, spaceMaxCm)
      .linExp(spaceMinCm, spaceMaxCm, spaceMinFreq, spaceMaxFreq))

    val vrGate = BooleanObj.newVar[T](true)

    pAttr.put("freq-seq", DoubleVector.newConst[T](freqSeq))
    pAttr.put("amp", DoubleObj.newConst[T](config.spaceAmp.dbAmp))
    pAttr.put("gate", vrGate)
    val r = Runner(p)
    r.run()
//    r.reactNow { implicit tx => state =>
//      if (state.idle) done(tx)
//    }

    new Result {
      override val runner: Runner[T] = r

      override def release()(implicit tx: T): Unit =
        vrGate() = false
    }
  }
}

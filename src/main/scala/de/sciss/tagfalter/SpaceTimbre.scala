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
import de.sciss.numbers.Implicits._
import de.sciss.proc.{Proc, Runner, TimeRef, Universe}
import de.sciss.synth.{Curve, SynthGraph}
import de.sciss.synth.UGenSource.Vec
import de.sciss.tagfalter.Main.{T, log}
import org.rogach.scallop
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object SpaceTimbre {

  case class ConfigImpl(
                         dur : Float = 30f,
                         numPos       : Int     = 5,
                         debug        : Boolean = false,
                         spaceMinCm   : Float   =    60.0f, // 10.0
                         spaceMaxCm   : Float   = 12000.0f, // 2000.0
                         spaceMinFreq : Float   =   150.0f,
                         spaceMaxFreq : Float   = 18000.0f,
                         spaceAmp     : Float   = -10.0f, // decibels
                         spaceCurve   : Curve   = Curve.exp,

                       ) extends Config

  trait Config {
    def debug       : Boolean
    def spaceMinCm  : Float
    def spaceMaxCm  : Float
    def spaceMinFreq: Float
    def spaceMaxFreq: Float
    def spaceAmp    : Float
    def spaceCurve  : Curve
  }

  private val curveNameMap: Map[String, Curve] = Map(
    "step"        -> Curve.step,
    "lin"         -> Curve.linear,
    "linear"      -> Curve.linear,
    "exp"         -> Curve.exponential,
    "exponential" -> Curve.exponential,
    "sin"         -> Curve.sine,
    "sine"        -> Curve.sine,
    "welch"       -> Curve.welch,
    "sqr"         -> Curve.squared,
    "squared"     -> Curve.squared,
    "cub"         -> Curve.cubed,
    "cubed"       -> Curve.cubed
  )

  implicit val ReadCurve: scallop.ValueConverter[Curve] = scallop.singleArgConverter { s =>
    curveNameMap.getOrElse(s.toLowerCase, {
      val p = s.toFloat
      Curve.parametric(p)
    })
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
      val spaceCurve: Opt[Curve] = opt(default = Some(default.spaceCurve),
        descr = s"Space-timbre frequency distribution curve; lin, exp, sine, welch, 2.0 etc. (default: ${default.spaceCurve}).",
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
        spaceCurve    = spaceCurve(),
      )
    }
    import p.config
    run()
  }

  trait Result {
    def runner: Runner[T]

    def release()(implicit tx: T): Unit
  }

  def aWeight(f: Double): Double = {
    val f2 = f .squared
    val f4 = f2.squared
    val nom = 12194.0.squared * f4
    val den = (f2 + 20.6.squared) * ((f2 + 107.7.squared) * (f2 + 737.9.squared)).sqrt * (f2 + 12194.squared)
    nom / den
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

  def apply(vec: Vec[Float])(implicit tx: T, config: Config, universe: Universe[T]): Result =
    applyWith(vec, amp = config.spaceAmp.dbAmp, skipFreq = Nil)

  /** @param skipFreq   frequencies to avoid, in ascending order
    */
  def applyWith(spacePosCm: Vec[Float], skipFreq: Seq[Float], amp: Float)(implicit tx: T, config: Config, universe: Universe[T]): Result = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.DoneSelf
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      val freqSeq = "freq-seq".kr(150.0) // Seq(1.0, 2.0))
      val ampSeq  = "amp-seq" .kr(1.0)
      //space.poll(0, "space")

      val oscAmpSeq = Vec.tabulate(spacePosCm.size) { i =>
        val n = LFNoise1.kr(i.linLin(0, spacePosCm.size - 1, 0.11, 0.23)).abs
        n * ampSeq.out(i)
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

    val spaceSeq = spacePosCm match {
      case Vec()        => Vec(120.0f, 5000.0f)
      case Vec(single)  => Vec(single, single * 1.5f)
      case _            => spacePosCm
    }
    val spaceLo   = spaceSeq.min
    val spaceHi   = spaceSeq.max
    log.debug(f"space-lo = $spaceLo%1.1f")
    log.debug(f"space-hi = $spaceHi%1.1f")

    import config.{spaceMaxCm, spaceMaxFreq, spaceMinCm, spaceMinFreq, spaceCurve}
    val freqSeq = spaceSeq.map { cm =>
      val cmClip  = cm/*.toDouble*/.clip(spaceMinCm, spaceMaxCm)
      // val f0      = cmClip.linExp(spaceMinCm, spaceMaxCm, spaceMinFreq, spaceMaxFreq)
      val pos     = cmClip.linLin(spaceMinCm, spaceMaxCm, 0.0f, 1.0f)
      val f0      = spaceCurve.levelAt(pos, y1 = spaceMinFreq, y2 = spaceMaxFreq)

      skipFreq.foldLeft(f0.toDouble) { (f1, fBlock) =>
        // XXX TODO: should we use relative frequencies, such as `fBlock * 0.99` ?
        //  I think Goertzel is on a linear spectrum, so probably fine like this
        val fBlockLo = fBlock - 100f
        val fBlockHi = fBlock + 100f
        if (f1 >= fBlockLo && f1 <= fBlockHi) fBlockHi else f1
      }
    }
    val ampSeq = freqSeq.map { f =>
      aWeight(f).max(0.5).reciprocal
    }

    val vrGate = BooleanObj.newVar[T](true)

    pAttr.put("freq-seq", DoubleVector.newConst[T](freqSeq))
    pAttr.put("amp-seq" , DoubleVector.newConst[T](ampSeq ))
    pAttr.put("amp", DoubleObj.newConst[T](amp /*config.spaceAmp.dbAmp*/))
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

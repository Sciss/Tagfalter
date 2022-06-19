/*
 *  Accelerate.scala
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

import de.sciss.lucre.synth.{Buffer => LBuffer}
import de.sciss.lucre.{BooleanObj, DoubleObj, DoubleVector, IntObj}
import de.sciss.numbers.Implicits._
import de.sciss.numbers.TwoPi
import de.sciss.proc.{Proc, Runner, Universe}
import de.sciss.synth.SynthGraph
import de.sciss.tagfalter.Main.{SR, T}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object Accelerate {
  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      printedName = "Tagfalter - Accelerate"
      private val default = ConfigImpl()

      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )
      val accelMicAmp: Opt[Float] = opt(default = Some(default.accelMicAmp),
        descr = s"Acceleration microphone boost, linear (default: ${default.accelMicAmp}).",
      )
      val accelSigAmp: Opt[Float] = opt(default = Some(default.accelSigAmp),
        descr = s"Acceleration signal boost, linear (default: ${default.accelSigAmp}).",
      )
      // --- impl ---
      val accelFactor: Opt[Float] = opt(default = Some(default.accelFactor),
        descr = s"Acceleration factor (default: ${default.accelFactor}).",
      )
      val accelBufDur: Opt[Float] = opt(default = Some(default.accelBufDur),
        descr = s"Acceleration buffer duration in seconds (default: ${default.accelBufDur}).",
      )
      val accelCmpThresh: Opt[Float] = opt(default = Some(default.accelCmpThresh),
        descr = s"Accelerate output compression threshold in neg.decibels (default: ${default.accelCmpThresh}).",
      )

      verify()
      implicit val config: ConfigImpl = ConfigImpl(
        debug           = debug(),
        accelMicAmp     = accelMicAmp(),
        accelSigAmp     = accelSigAmp(),
        accelFactor     = accelFactor(),
        accelBufDur     = accelBufDur(),
        accelCmpThresh  = accelCmpThresh(),
      )
    }
    import p.config
    run()
  }

  final val filterLen  = 512

  case class ConfigImpl(
                       // base
                       accelBufDur    : Float   = 12.0f,
                       accelFactor    : Float   = 32f,
                       // core
                       debug          : Boolean = false,
                       accelMicAmp    : Float   = 10.0f,
                       accelSigAmp    : Float   =  1.0f,
                       accelCmpThresh : Float   = 15f,
                       ) extends Config

  trait Config {
    def debug         : Boolean
    def accelMicAmp   : Float
    def accelSigAmp   : Float
    def accelCmpThresh: Float
  }

//  trait Config extends Config{
//    def accelBufDur : Float
//    def accelFactor : Float
//  }

  def run()(implicit config: ConfigImpl): Unit = {
//    val bla = Runtime.getRuntime ().freeMemory ()
//    println(s"FREE MEM : $bla")
//    sys.exit()

//    val nyquist = SR / 2
//    val factor  = 32.0
//    val rollOff = 0.8
//    val f       = nyquist * rollOff / factor
//    val kernel  = makeLPF(f)
//    val afOut   = AudioFile.openWrite(userHome / "Documents" / "temp" / "_killme.aif",
//      AudioFileSpec(numChannels = 1, sampleRate = SR)
//    )
//    afOut.write(Array(kernel))
//    afOut.close()

    Main.boot { implicit tx => implicit universe => _ /*s*/ =>
      accelImpl(/*s*/)
    }
  }

  def accelImpl(/*s: Server*/)(implicit tx: T, universe: Universe[T], config: ConfigImpl): Unit = {
    val rc = recWith(
      accelFactor = config.accelFactor,
      accelBufDur = config.accelBufDur
    )
    play(rc)
  }

  trait RecResult {
    def buffer: LBuffer

    def runner: Runner[T]

    def release()(implicit tx: T): Unit
  }

  trait PlayResult {
    def runner: Runner[T]

    def release()(implicit tx: T): Unit
  }

  def play(rc: RecResult)(implicit tx: T, universe: Universe[T], config: Config): PlayResult = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}

      val gate          = "gate".kr(1)
      val bufAccel      = "buf".kr(0) // Buffer.Empty(accelFrames)
      val sigRd         = PlayBuf.ar(1, bufAccel, loop = 1)

      val cmpRatioOut   = 1.0/8 // "cmp-ratio-in".kr(1.0/8)
      val cmpThreshOut  = "cmp-thresh-out".kr((-24).dbAmp)
      val cmpOut        = Compander.ar(sigRd, sigRd, thresh = cmpThreshOut, ratioBelow = 1.0, ratioAbove = cmpRatioOut,
        attack = 0.01, release = 10.0)

      val amp         = "amp".kr(1.0)
      val env         = EnvGen.kr(Env.asr(attack = 2.5f, level = amp, release = 5f), gate = gate)
      DoneSelf(Done.kr(env))
      val sig         = cmpOut * env

      if (config.debug) {
        sig.poll(1, "accel-play")
      }

      PhysicalOut.ar(0, sig)
    }

    val p = Proc[T]()
    val vrGate = BooleanObj.newVar[T](true)
    p.graph()     = g
    val pAttr     = p.attr
    pAttr.put("amp"     , DoubleObj.newConst[T](config.accelSigAmp))
    pAttr.put("buf"     , IntObj.newConst[T](rc.buffer.id))  // XXX TODO: yeah, well, we need a proc.Buffer object
    pAttr.put("cmp-thresh-out", DoubleObj.newConst[T]((-config.accelCmpThresh).dbAmp))
    pAttr.put("gate"    , vrGate)
    val r = Runner(p)
    r.run()

    new PlayResult {
      override val runner: Runner[T]  = r

      override def release()(implicit tx: T): Unit =
        vrGate() = false
    }
  }

  def recWith(accelFactor: Float, accelBufDur: Float)(implicit tx: T, universe: Universe[T], config: Config): RecResult = {
    val s = universe.auralContext.get.server

    val nyquist     = SR / 2
    // val accelFactor      = config.accelFactor // 32.0
    val rollOff     = 0.8
    val cutOff      = nyquist * rollOff / accelFactor
    val accelFrames = (accelBufDur * SR).toInt

    val b = LBuffer(s)(numFrames = accelFrames)

    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}

      val kernel      = Buffer("kernel")
      val in0         = PhysicalIn.ar
      val in          = in0 * "mic-amp".kr(1.0)
      val flt         = Convolution2.ar(in, kernel, frameSize = filterLen)

      if (config.debug) {
        in.poll(1, "accel-rec")
      }

      val bufAccel    = "buf".kr(0) // Buffer.Empty(accelFrames)
      val sigWr       = flt
      val indexWr     = Phasor.ar(speed = accelFactor.reciprocal, lo = 0, hi = accelFrames)
      BufWr.ar(sigWr, bufAccel, index = indexWr, loop = 1)

//      val sigRd       = PlayBuf.ar(1, bufAccel, loop = 1)
//      val sig         = sigRd * "amp".kr(1.0)
//      PhysicalOut.ar(0, sig)
    }

    val p = Proc[T]()
    p.graph()     = g
    val pAttr     = p.attr
    val arrKernel = makeLPF(cutOff)
    val vecKernel = DoubleVector.newConst[T](arrKernel.toIndexedSeq)
    pAttr.put("kernel", vecKernel)
    pAttr.put("mic-amp" , DoubleObj.newConst[T](config.accelMicAmp))
//    pAttr.put("amp"     , DoubleObj.newConst[T](config.accelSigAmp))
    pAttr.put("buf"     , IntObj.newConst[T](b.id))  // XXX TODO: yeah, well, we need a proc.Buffer object
    val r = Runner(p)
    r.run()

    new RecResult {
      override val buffer: LBuffer    = b
      override val runner: Runner[T]  = r

      override def release()(implicit tx: T): Unit = r.stop()
    }
  }

  private def makeLPF(f: Double): Array[Double] = {
    val f1N     = f / SR
    val kernel  = new Array[Double](filterLen)
    val kaiser  = 6.0
    fillSinc (winSize = filterLen, winOff = 0, buf = kernel, bufOff = 0, len = filterLen, param = f1N)
    mulKaiser(winSize = filterLen, winOff = 0, buf = kernel, bufOff = 0, len = filterLen, param = kaiser,
      gain = 2 * f1N)
    kernel
  }

  private def fillSinc(winSize: Int, winOff: Int, buf: Array[Double], bufOff: Int, len: Int, param: Double): Unit = {
    val radius  = 0.5 * winSize
    val norm    = param * TwoPi
    var i       = winOff
    val stop    = i + len
    var j       = bufOff
    while (i < stop) {
      val d  = (i - radius) * norm
      buf(j) = if (d == 0.0) 1.0 else math.sin(d) / d
      i += 1
      j += 1
    }
  }

  private def mulKaiser(winSize: Int, winOff: Int, buf: Array[Double], bufOff: Int, len: Int, param: Double,
                        gain: Double): Unit = {
    val norm  = 2.0 / winSize
    val iBeta = gain / calcBesselZero(param)
    var i     = winOff
    val stop  = i + len
    var j     = bufOff
    while (i < stop) {
      val d  = i * norm - 1
      buf(j) *= calcBesselZero(param * math.sqrt(1.0 - d * d)) * iBeta
      i += 1
      j += 1
    }
  }

  private def calcBesselZero(x: Double): Double = {
    var d2  = 1.0
    var sum = 1.0
    var n   = 1
    val xh  = x * 0.5

    while ({
      val d1 = xh / n
      n += 1
      d2 *= d1 * d1
      sum += d2

      d2 >= sum * 1e-21 // precision is 20 decimal digits
    }) ()

    sum
  }

//  private def createAntiAliasFilter(impResp: Array[Float], halfWinSize: Int, rollOff: Double,
//                                    kaiserBeta: Double, samplesPerCrossing: Int, step: Int): Unit =
//    createLPF(impResp, 0.5 * rollOff, halfWinSize = halfWinSize, kaiserBeta = kaiserBeta,
//      samplesPerCrossing = samplesPerCrossing, step = step)
}

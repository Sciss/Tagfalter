/*
 *  Crypsis.scala
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

import de.sciss.audiofile.AudioFileSpec
import de.sciss.lucre.{Artifact, ArtifactLocation, BooleanObj, DoubleObj}
import de.sciss.numbers.Implicits.floatNumberWrapper
import de.sciss.proc.{AudioCue, Proc, Runner, Universe}
import de.sciss.synth.SynthGraph
import de.sciss.tagfalter.Main.{SR, T}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.File

object Crypsis {

  case class ConfigImpl(
                         debug          : Boolean =  false,
                         debugRec       : Boolean =  false,
                         crypMicAmp     : Float   = 40.0f, // 4.0f,
                         crypSpeakerAmp : Float   = 12.0f,
                         cmpThreshIn    : Float   =  10f,
                         cmpThreshOut   : Float   =  15f,
                         crypAchilles   : Float   =  0.98f,
                         crypModFreq    : Float   =  0.15f, // 0.5f, // 5.6f,
//                         crypModDepth   : Float   = 0.7f,
                         dirAudio       : File    = new File("audio_work"),
                       ) extends Config

  trait Config {
    def debug           : Boolean
    def debugRec        : Boolean
    def crypMicAmp      : Float
    def crypSpeakerAmp  : Float
    def cmpThreshIn     : Float
    def cmpThreshOut    : Float
    def crypAchilles    : Float
    def crypModFreq     : Float
//    def crypModDepth    : Float
    def dirAudio        : File
  }

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      printedName = "Tagfalter - Crypsis"
      private val default = ConfigImpl()

      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity).",
      )
      val debugRec: Opt[Boolean] = toggle(default = Some(default.debugRec),
        descrYes = "Enter debug mode (control files).",
      )
      val crypMicAmp: Opt[Float] = opt(default = Some(default.crypMicAmp),
        descr = s"Crypsis microphone boost, decibels (default: ${default.crypMicAmp}).",
      )
      val crypSpeakerAmp: Opt[Float] = opt(default = Some(default.crypSpeakerAmp),
        descr = s"Crypsis speaker amplitude, decibels (default: ${default.crypSpeakerAmp}).",
      )
      val cmpThreshIn: Opt[Float] = opt(default = Some(default.cmpThreshIn),
        descr = s"Crypsis input compression threshold in neg.decibels (default: ${default.cmpThreshIn}).",
      )
      val cmpThreshOut: Opt[Float] = opt(default = Some(default.cmpThreshOut),
        descr = s"Crypsis output compression threshold in neg.decibels (default: ${default.cmpThreshOut}).",
      )
      val crypAchilles: Opt[Float] = opt(default = Some(default.crypAchilles),
        descr = s"Crypsis achilles factor (default: ${default.crypAchilles}).",
        validate = x => x > 0.7 && x < 1.5
      )
      val crypModFreq: Opt[Float] = opt(default = Some(default.crypModFreq),
        descr = s"Crypsis amplitude modulation frequency in Hz (default: ${default.crypModFreq}).",
      )
      val dirAudio: Opt[File] = opt(default = Some(default.dirAudio),
        descr = s"Audio file directory (default: ${default.dirAudio})"
      )
//      val crypModDepth: Opt[Float] = opt(default = Some(default.crypModDepth),
//        descr = s"Crypsis amplitude modulation depth 0 to 1 (default: ${default.crypModDepth}).",
//        validate = x => x >= 0.0 && x <= 1.0
//      )

      verify()
      implicit val config: Config = ConfigImpl(
        debug           = debug(),
        debugRec        = debugRec(),
        crypMicAmp      = crypMicAmp(),
        crypSpeakerAmp  = crypSpeakerAmp(),
        cmpThreshIn     = cmpThreshIn(),
        cmpThreshOut    = cmpThreshOut(),
        crypAchilles    = crypAchilles(),
        crypModFreq     = crypModFreq(),
//        crypModDepth    = crypModDepth(),
        dirAudio        = dirAudio().getAbsoluteFile,
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    Main.boot { implicit tx => implicit universe => s =>
      apply(/*s*/)
      ()
    }
  }

  final val LATENCY_MIN = 4096  // minimum without mic-to-speaker spacing
//  final val LATENCY_ADD = 141   // 1m spacing
  final val LATENCY_ACOUSTIC_SEC = 0.003 // 1m spacing
  final val LAG_TIME_UP_SEC = 0.1
  final val LAG_TIME_DN_SEC = 1.5 // 0.5 // 0.3

  trait Result {
    def runner: Runner[T]

    def release()(implicit tx: T): Unit
  }

  def apply(/*s: Server*/)(implicit tx: T, config: Config, universe: Universe[T]): Result = {
    applyWith(modFreq = config.crypModFreq)
  }

  def applyWith(modFreq: Float)(implicit tx: T, config: Config, universe: Universe[T]): Result = {
    val p = Proc[T]()
    val dirAudio  = config.dirAudio
    val locAudio  = ArtifactLocation.newConst[T](dirAudio.toURI)
    val artNoise  = Artifact[T](locAudio, Artifact.Child("whitenoise2s_48kHz.aif"))
    val specNoise = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 2 * SR)
    val cueNoise  = AudioCue.Obj[T](artNoise, specNoise, 0L, 1.0)

    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, DiskOut => _, PartConv => _, _}
      val in0       = PhysicalIn.ar
      val in1       = in0 * "mic-amp".kr(1.0)
      val in2       = in1 // HPF.ar(in1, 100)
//      val latencyFr =

      val modFreq         = "mod-freq".kr(5.0)
      val modPeriod       = 0.5 / modFreq
      val pulseWidth0     = (0.5 * modPeriod - (LATENCY_ACOUSTIC_SEC /**2*/ + (LAG_TIME_UP_SEC + LAG_TIME_DN_SEC * 1.5) * 0.5)) / modPeriod // e.g.  0.44
      val pulseWidth      = pulseWidth0.max(0.05)

      /*

       iphase: initial phase offset in cycles ( 0..1 ). If you think of a buffer of one cycle of the waveform, this is
       the starting offset into this buffer. Hence, an iphase of 0.25 means that you will hear the first impulse after
       0.75 periods! If you prefer to specify the perceived delay instead, you could use an iphase of -0.25 + 1
       which is more intuitive. Note that the phase is not automatically wrapped into the range of 0..1 , so
       putting an iphase of -0.25 currently results in a strange initial signal which only stabilizes to the correct
       behaviour after one period! (init-time only)

       */
      val LATENCY_TOTAL   = -(LATENCY_MIN.toDouble / SR + LATENCY_ACOUSTIC_SEC) + (LAG_TIME_UP_SEC + LAG_TIME_DN_SEC) * 0.7
//      println(s"LATENCY_TOTAL $LATENCY_TOTAL")
      val pulseInOutDly   = LATENCY_TOTAL / modPeriod // periods
      val pulseInPhase    = 0.0
      val pulseOutPhase   = (pulseInOutDly + 0.5).wrap(0.0, 1.0)

      val pulseIn         = LFPulse.ar(modFreq, iphase = pulseInPhase   , width = pulseWidth)
      val pulseOut        = LFPulse.ar(modFreq, iphase = pulseOutPhase  , width = pulseWidth)
      val pulseInL        = Lag2UD.ar(pulseIn , LAG_TIME_UP_SEC, LAG_TIME_DN_SEC)
      val pulseOutL       = Lag2UD.ar(pulseOut, LAG_TIME_UP_SEC, LAG_TIME_DN_SEC * 2)
      val envLPF          = Env.asr(attack = LAG_TIME_UP_SEC * 3, release = LAG_TIME_DN_SEC * 2)
      val egLPF           = EnvGen.kr(envLPF, gate = pulseOut).linExp(0.0, 1.0, 50, 16000)

      val in        = in2 * pulseInL

      //      Lag.ar()

      if (config.debug) Amplitude.ar(in).ampDb.poll(4, "amp-in")
      val cmpThreshIn = "cmp-thresh-in".kr((-24).dbAmp)
//      if (config.debug) cmpThreshIn.poll(0, "cmpThreshIn")
      val cmpRatioIn  = 1.0/8 // "cmp-ratio-in".kr(1.0/8)
      val cmpIn     = Compander.ar(in, in, thresh = cmpThreshIn, ratioBelow = 1.0, ratioAbove = cmpRatioIn,
        attack = 0.01, release = 10.0)
      val sigIn     = cmpIn
//      val dlyTime   = 10.0
      val fftSize   = 2048
      val fuzzyAmp  = 2.0 / fftSize
      val fuzzy     = PartConv.ar("noise", sigIn, fftSize = fftSize) * fuzzyAmp
      if (config.debug) Amplitude.ar(fuzzy).ampDb.poll(4, "amp-out")
      val cmpThreshOut = "cmp-thresh-out".kr((-24).dbAmp)
      if (config.debug) cmpThreshOut.poll(0, "cmpThreshOut")
      val cmpRatioOut = 1.0/8 // "cmp-ratio-in".kr(1.0/8)
      val cmpOut    = Compander.ar(fuzzy, fuzzy, thresh = cmpThreshOut, ratioBelow = 1.0, ratioAbove = cmpRatioOut,
        attack = 0.01, release = 10.0)
      val dlyTime   = 7.5 // 10.0
      val dly       = DelayN.ar(cmpOut, dlyTime, dlyTime)

//      def mkAchil(in: GE): GE = {
//        val speed       = "achilles".kr(0.98) // 1.03 // 1.02
////        if (config.debug) speed.poll(0, "achilles")
//        val numFrames   = 2 * SR // SR // SampleRate.ir // sampleRate.toInt
//        val bufAchil    = LocalBuf(numFrames = numFrames) // , numChannels = Pad(1, in))
//        ClearBuf(bufAchil)
//        val writeRate   = 1.0 // BufRateScale.kr(bufID)
//        val readRate    = writeRate * speed
//        val readPhasor  = Phasor.ar(0, readRate, 0, numFrames)
//        val read0       = BufRd.ar(1, bufAchil, readPhasor, 0, 4)
//        val read        = read0 // Gate.ar(read0, readBad sig_== 0)
//        val writePhasor = Phasor.ar(0, writeRate, 0, numFrames)
//        val old         = BufRd.ar(1, bufAchil, writePhasor, 0, 1)
//        val wet0        = SinOsc.ar(0, (readPhasor - writePhasor).abs / numFrames * math.Pi)
//        val dry         = 1 - wet0.squared
//        val wet         = 1 - (1 - wet0).squared
//        val write0      = (old * dry) + (in * wet)
//        // val writeBad    = CheckBadValues.ar(write0, id = 1001)
//        val writeSig    = write0 // Gate.ar(write0, writeBad sig_== 0)
//
//        // NOTE: `writeSig :: Nil: GE` does _not_ work because single
//        // element seqs are not created by that conversion.
//        BufWr.ar(/*Pad.Split(*/writeSig/*)*/, bufAchil, writePhasor)
//        read
//      }

      val achil     = dly // mkAchil(dly)
//      val modDepth  = "mod-freq".kr(0.8)
      val mod0      = achil * pulseOutL // SinOsc.kr(modFreq).mulAdd(modDepth * 0.5, 1.0 - modDepth * 0.5)
      val mod       = LPF.ar(mod0, egLPF)
      val sig       = mod * "amp".kr(1)
      PhysicalOut.ar(0, sig)

      val gate      = "gate".kr(1)
      val done      = !gate & A2K.kr(pulseOutL) < (-40).dbAmp
      DoneSelf(done)

//      // some debugging:
//      modFreq       .poll(0, "CR modFreq")
//      pulseWidth    .poll(0, "CR pulseWidth")
//      pulseOutPhase .poll(0, "CR pulseOutPhase")
//      in            .poll(1, "CR in")
//      sig           .poll(1, "CR sig")

      if (config.debugRec) {
        DiskOut.ar("rec", Seq(
          pulseIn,
          pulseOut,
          pulseInL,
          pulseOutL,
        ))
      }
    }

    p.graph() = g
    val pAttr = p.attr
    pAttr.put("noise"         , cueNoise)
    pAttr.put("amp"           , DoubleObj.newConst[T](config.crypSpeakerAmp .dbAmp))
    pAttr.put("mic-amp"       , DoubleObj.newConst[T](config.crypMicAmp     .dbAmp))
    pAttr.put("cmp-thresh-in" , DoubleObj.newConst[T]((-config.cmpThreshIn ).dbAmp))
    pAttr.put("cmp-thresh-out", DoubleObj.newConst[T]((-config.cmpThreshOut).dbAmp))
    pAttr.put("achilles"      , DoubleObj.newConst[T](config.crypAchilles))
    pAttr.put("mod-freq"      , DoubleObj.newConst[T](modFreq))
//    pAttr.put("mod-depth"     , DoubleObj.newConst[T](config.crypModDepth))
    val vrGate = BooleanObj.newVar[T](true)
    pAttr.put("gate"          , vrGate)

    if (config.debugRec) {
      val artRec = Artifact[T](locAudio, Artifact.Child("_killme.irc"))
      pAttr.put("rec", artRec)
    }

    val r = Runner(p)
    r.run()

    new Result {
      override val runner: Runner[T] = r

      override def release()(implicit tx: T): Unit =
        vrGate() = false
    }
  }
}

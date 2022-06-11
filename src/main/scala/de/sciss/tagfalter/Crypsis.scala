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
import de.sciss.file.userHome
import de.sciss.lucre.{Artifact, ArtifactLocation, DoubleObj}
import de.sciss.numbers.Implicits.floatNumberWrapper
import de.sciss.proc.{AudioCue, Proc, Runner, Universe}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.{GE, SynthGraph}
import de.sciss.tagfalter.Main.{SR, T}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.File

object Crypsis {

  case class ConfigImpl(
                         debug          : Boolean = false,
                         crypMicAmp     : Float   = 30.0f, // 4.0f,
                         crypSpeakerAmp : Float   =  4.0f,
                         cmpThreshIn    : Float   = -15f,
                         cmpThreshOut   : Float   = -15f,
                         crypAchilles   : Float   = 0.98f,
                         crypModFreq    : Float   = 5.6f,
                         crypModDepth   : Float   = 0.7f,
                       ) extends Config

  trait Config {
    def debug           : Boolean
    def crypMicAmp      : Float
    def crypSpeakerAmp  : Float
    def cmpThreshIn     : Float
    def cmpThreshOut    : Float
    def crypAchilles    : Float
    def crypModFreq     : Float
    def crypModDepth    : Float
  }

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      printedName = "Tagfalter - Crypsis"
      private val default = ConfigImpl()

      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )
      val crypMicAmp: Opt[Float] = opt(default = Some(default.crypMicAmp),
        descr = s"Crypsis microphone boost, linear (default: ${default.crypMicAmp}).",
      )
      val crypSpeakerAmp: Opt[Float] = opt(default = Some(default.crypSpeakerAmp),
        descr = s"Crypsis speaker amplitude, linear (default: ${default.crypSpeakerAmp}).",
      )
      val cmpThreshIn: Opt[Float] = opt(default = Some(default.cmpThreshIn),
        descr = s"Crypsis input compression threshold in decibels (default: ${default.cmpThreshIn}).",
      )
      val cmpThreshOut: Opt[Float] = opt(default = Some(default.cmpThreshOut),
        descr = s"Crypsis output compression threshold in decibels (default: ${default.cmpThreshOut}).",
      )
      val crypAchilles: Opt[Float] = opt(default = Some(default.crypAchilles),
        descr = s"Crypsis achilles factor (default: ${default.crypAchilles}).",
        validate = x => x > 0.7 && x < 1.5
      )
      val crypModFreq: Opt[Float] = opt(default = Some(default.crypModFreq),
        descr = s"Crypsis amplitude modulation frequency in Hz (default: ${default.crypModFreq}).",
      )
      val crypModDepth: Opt[Float] = opt(default = Some(default.crypModDepth),
        descr = s"Crypsis amplitude modulation depth 0 to 1 (default: ${default.crypModDepth}).",
        validate = x => x >= 0.0 && x <= 1.0
      )

      verify()
      implicit val config: Config = ConfigImpl(
        debug           = debug(),
        crypMicAmp      = crypMicAmp(),
        crypSpeakerAmp  = crypSpeakerAmp(),
        cmpThreshIn     = cmpThreshIn(),
        cmpThreshOut    = cmpThreshOut(),
        crypAchilles    = crypAchilles(),
        crypModFreq     = crypModFreq(),
        crypModDepth    = crypModDepth(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    Main.boot { implicit tx => implicit universe => s =>
      apply(/*s*/)
    }
  }

  def apply(/*s: Server*/)(implicit tx: T, config: Config, universe: Universe[T]): Unit = {
    val p = Proc[T]()
    val dirAudio  = new File(userHome, "Documents/projects/Klangnetze/audio_work")
    val locAudio  = ArtifactLocation.newConst[T](dirAudio.toURI)
    val artNoise  = Artifact[T](locAudio, Artifact.Child("whitenoise2s_48kHz.aif"))
    val specNoise = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 2 * SR)
    val cueNoise  = AudioCue.Obj[T](artNoise, specNoise, 0L, 1.0)

    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
//      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.proc.graph.Ops.stringToControl
      val in0       = PhysicalIn.ar
      val in1       = in0 * "mic-amp".kr(1.0)
      val in        = HPF.ar(in1, 100)
      if (config.debug) Amplitude.ar(in).ampDb.poll(4, "amp-in")
      val cmpThreshIn = "cmp-thresh-in".kr(-24.dbAmp)
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
      val cmpThreshOut = "cmp-thresh-out".kr(-24.dbAmp)
      if (config.debug) cmpThreshOut.poll(0, "cmpThreshOut")
      val cmpRatioOut = 1.0/8 // "cmp-ratio-in".kr(1.0/8)
      val cmpOut    = Compander.ar(fuzzy, fuzzy, thresh = cmpThreshOut, ratioBelow = 1.0, ratioAbove = cmpRatioOut,
        attack = 0.01, release = 10.0)
      val dlyTime   = 10.0
      val dly       = DelayN.ar(cmpOut, dlyTime, dlyTime)

      def mkAchil(in: GE): GE = {
        val speed       = "achilles".kr(0.98) // 1.03 // 1.02
//        if (config.debug) speed.poll(0, "achilles")
        val numFrames   = 2 * SR // SR // SampleRate.ir // sampleRate.toInt
        val bufAchil    = LocalBuf(numFrames = numFrames) // , numChannels = Pad(1, in))
        ClearBuf(bufAchil)
        val writeRate   = 1.0 // BufRateScale.kr(bufID)
        val readRate    = writeRate * speed
        val readPhasor  = Phasor.ar(0, readRate, 0, numFrames)
        val read0       = BufRd.ar(1, bufAchil, readPhasor, 0, 4)
        val read        = read0 // Gate.ar(read0, readBad sig_== 0)
        val writePhasor = Phasor.ar(0, writeRate, 0, numFrames)
        val old         = BufRd.ar(1, bufAchil, writePhasor, 0, 1)
        val wet0        = SinOsc.ar(0, (readPhasor - writePhasor).abs / numFrames * math.Pi)
        val dry         = 1 - wet0.squared
        val wet         = 1 - (1 - wet0).squared
        val write0      = (old * dry) + (in * wet)
        // val writeBad    = CheckBadValues.ar(write0, id = 1001)
        val writeSig    = write0 // Gate.ar(write0, writeBad sig_== 0)

        // NOTE: `writeSig :: Nil: GE` does _not_ work because single
        // element seqs are not created by that conversion.
        BufWr.ar(/*Pad.Split(*/writeSig/*)*/, bufAchil, writePhasor)
        read
      }

      val achil     = mkAchil(dly)
      val modFreq   = "mod-freq".kr(5.0)
      val modDepth  = "mod-freq".kr(0.8)
      val mod       = achil * SinOsc.kr(modFreq).mulAdd(modDepth * 0.5, 1.0 - modDepth * 0.5)
      val sig       = mod * "amp".kr(1)
      PhysicalOut.ar(0, sig)
    }

    p.graph() = g
    val pAttr = p.attr
    pAttr.put("noise"         , cueNoise)
    pAttr.put("amp"           , DoubleObj.newConst[T](config.crypSpeakerAmp))
    pAttr.put("mic-amp"       , DoubleObj.newConst[T](config.crypMicAmp))
    pAttr.put("cmp-thresh-in" , DoubleObj.newConst[T](config.cmpThreshIn  .dbAmp))
    pAttr.put("cmp-thresh-out", DoubleObj.newConst[T](config.cmpThreshOut .dbAmp))
    pAttr.put("achilles"      , DoubleObj.newConst[T](config.crypAchilles))
    pAttr.put("mod-freq"      , DoubleObj.newConst[T](config.crypModFreq))
    pAttr.put("mod-depth"     , DoubleObj.newConst[T](config.crypModDepth))
    pAttr.put("mic-amp"       , DoubleObj.newConst[T](config.crypMicAmp))

    val r = Runner(p)
    r.run()
  }
}

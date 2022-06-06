/*
 *  Biphase.scala
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

import de.sciss.lucre.synth.{Buffer, RT, Server, Synth}
import de.sciss.proc.TimeRef
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.{GE, SynthGraph}
import de.sciss.tagfalter.Main.SR
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object Biphase {
  case class ConfigImpl(
                         bitPeriod: Float   = 120.0f,
                         debug    : Boolean = false,
                         encAmp   : Float   = 0.1f,
                         decAmp2  : Float   = 0.3f, // 0.5f,
                         decMicAmp: Float   = 20.0f, // 4.0f,
                         program  : String  = "enc"
                       ) extends Config

  trait Config {
    def bitPeriod : Float
    def debug     : Boolean
    def encAmp    : Float
    def decAmp2   : Float // why oh why?
    def decMicAmp : Float
    def program   : String
  }

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      import org.rogach.scallop._

      printedName = "Tagfalter - CalibrateGoertzel"
      private val default = ConfigImpl()

      val bitPeriod: Opt[Float] = opt(default = Some(default.bitPeriod),
        descr = s"Bit encoding period in milliseconds (default: ${default.bitPeriod}).",
      )
      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )
      val encAmp: Opt[Float] = opt(default = Some(default.encAmp),
        descr = s"Bit encoding amplitude, linear (default: ${default.encAmp}).",
      )
      val decAmp2: Opt[Float] = opt(default = Some(default.decAmp2),
        descr = s"Bit decoding amplitude for second frequency, linear (default: ${default.decAmp2}).",
      )
      val decMicAmp: Opt[Float] = opt(default = Some(default.decMicAmp),
        descr = s"Bit decoding microphone boost, linear (default: ${default.decMicAmp}).",
      )
      val program: Opt[String] = opt(default = Some(default.program),
        descr = s"Program, one of 'enc', 'dec', 'both' (default: ${default.program}).",
        validate = s => s == "enc" || s == "dec" || s == "both"
      )

      verify()
      implicit val config: Config = ConfigImpl(
        bitPeriod = bitPeriod(),
        debug     = debug(),
        encAmp    = encAmp(),
        decAmp2   = decAmp2(),
        decMicAmp = decMicAmp(),
        program   = program(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    Main.boot { implicit tx => implicit universe => s =>
      val mac   = Main.macAddress()
      val bytes = 0x48.toByte +: mac
      val sch = universe.scheduler
      sch.schedule(sch.time + TimeRef.SampleRate.toLong) { implicit tx =>
        if (config.debug) s.peer.dumpOSC()
        if (config.program == "dec" || config.program == "both") {
          receive(s)
        }
        if (config.program == "enc" || config.program == "both") {
          send(s, bytes)
        }
      }
    }
  }

  // "global"
  final val f1a       =  4240.0f
  final val f1b       =  7680.0f
  final val f2a       = 11120.0f
  final val f2b       = 14560.0f

  final val NumIdle   = 4

  private final val startBit  = 0
  private final val stopBit   = 1
  private final val idle      = 1

  def send(s: Server, bytes: Array[Byte])(implicit tx: RT, config: Config): Unit = {
    val bits0: Array[Float] = bytes.flatMap { b =>
      val pay = Seq.tabulate(8) { i =>
        ((b >> (7 - i)) & 1).toFloat
      }
      startBit.toFloat +: pay :+ stopBit.toFloat
    }
    //val bits = Seq.fill(50)(idle) ++ bits0
    val bits: Vec[Float] = Vec.fill(NumIdle)(idle.toFloat) ++ bits0

    if (config.debug) println(bits.mkString("Biphase Send Bits: ", ", ", ""))

    val numBits = bits.size

    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}

      val bitPeriod = config.bitPeriod //  160.0 // 80.0 //  "bit-period".kr(40.0) // ms
      val bitFreq   = 1000.0f / bitPeriod

      //val bytesArr = macAddress("wlp0s20f3")

      //val tx = "tx".tr

      val bitBuf = "bit-buf".kr // LocalBuf(numBits)
//      SetBuf(bitBuf, bits)

      val bitClock   = Impulse.kr(bitFreq) // Impulse.ar(bitFreq)
      val phaseClock = Impulse.kr(bitFreq, 0.5f) // Impulse.ar(bitFreq, 0.5)

      //val bufRdIdx = PulseCount.ar(bitClock).min(numBits) - 1
      val bufRdIdx = PulseCount.kr(bitClock).min(numBits) - 1
      //val currentBit = BufRd.ar(1, bitBuf,
      //  index = bufRdIdx, loop = 0, interp = 0)
      val currentBit = BufRd.kr(1, bitBuf,
        index = bufRdIdx, loop = 0, interp = 0)

      currentBit.poll(bitClock, "BIT")

      val level = ToggleFF.ar(bitClock | (currentBit & phaseClock))

      val LAG = true
      val LAG_TIME = 0.01f

      val f1A  = 1.0f - level
      val f1AL = if (!LAG) f1A else Lag.ar(f1A, LAG_TIME)
      val f2A  = level
      val f2AL = if (!LAG) f2A else Lag.ar(f2A, LAG_TIME)
      val oscA = SinOsc.ar(f1a) * f1AL + SinOsc.ar(f2a) * f2AL
      val oscB = SinOsc.ar(f1b) * f1AL + SinOsc.ar(f2b) * f2AL

      val phasePeriodS = bitPeriod / 500
      val osc = oscA + DelayN.ar(oscB, phasePeriodS, phasePeriodS)

      val amp = "amp".kr(0.1f)
      PhysicalOut.ar(0, osc * amp)
    }

    val bitBuf = Buffer(s)(numFrames = numBits)
    bitBuf.setn(bits)
    val syn = Synth.play(g, nameHint = Some("bi-enc"))(s,
      args = Seq("bit-buf" -> bitBuf.id, "amp" -> config.encAmp),
      dependencies = bitBuf :: Nil)
    syn.onEndTxn { implicit tx =>
      bitBuf.dispose()
    }
  }


  def receive(s: Server)(implicit tx: RT, config: Config): Unit = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      // version: 06-Jun-2022

      val f1        = f1a // 4240.0 // "freq-space".kr(1000) // Hz
      val f2        = f2a // 11120.0 // "freq-mark" .kr(1200) // Hz
      val bitPeriod = config.bitPeriod // 120.0 // 160.0 // 80.0 // "bit-period".kr(40.0) // ms
      val CD        = ControlDur.ir
      val bitLen    = bitPeriod / 1000.0 * SR
      //val phaseDur  = bitPeriod / 2000.0
      val phaseLen  = (bitLen/2).roundTo(1)
      //val bitFreq   = 1000.0 / bitPeriod
      //960/4
      //phaseLen.poll(0, "phaseLen")

      val in0       = PhysicalIn.ar
      val in        = in0 * "mic-amp".kr(1.0)
      val gLen      = 1024 // phaseLen / 8 // 4 // 256 // 512 // 1024 // * 2 // phaseLen // 1024
      val gLenH     = gLen/2
      //val gLenC     = (gLen/2).squared
      val gHop      = 1.0 // 0.5 // 0.25 // 1.0
      val gMedian   = 5 // 9

      val gGain1 = 1.0f
      val gGain2 = config.decAmp2 // 0.25

      val g1        = Goertzel.kr(in, gLen, freq = f1, hop = gHop)
      val g2        = Goertzel.kr(in, gLen, freq = f2, hop = gHop)
      val g1M0      = (g1.real.squared + g1.imag.squared).sqrt / gLenH * gGain1
      val g2M0      = (g2.real.squared + g2.imag.squared).sqrt / gLenH * gGain2
      val g1M       = Median.kr(g1M0, length = gMedian)
      val g2M       = Median.kr(g2M0, length = gMedian)

      val threshAbs = "thresh".kr(-48.dbAmp)
      //val threshSq  = threshAbs.squared

      //threshSq.poll(0, "threshSq")
      if (config.debug) g1M.poll(2, "g1M")
      if (config.debug) g2M.poll(2, "g2M")

      val gRatio    = g1M / (g2M.max(1.0e-6))
      val g1T       = g1M > threshAbs & gRatio > 1.2 // 1.4 // 1.5
      val g2T       = g2M > threshAbs & gRatio < (1.0 / 1.2) // (1.0/1.5)
      val isEdge    = Trig.kr(g1T, CD) | Trig.kr(g2T, CD)

      val edgePeriod = Timer.kr(isEdge) * 1000 // ms
      val midPeriod  = bitPeriod * (3.0/4)
      val outPeriod  = bitPeriod * (5.0/4)
      val inPeriod   = bitPeriod * (1.0/4)
      //midPeriod.poll(0, "midPeriod")
      //outPeriod.poll(0, "outPeriod")
      val isShortEdge = isEdge & edgePeriod <  midPeriod & edgePeriod >= inPeriod
      val isLongEdge  = isEdge & edgePeriod >= midPeriod & edgePeriod <= outPeriod
      //val isBadEdge   = isEdge & !(isShortEdge | isLongEdge)

      //g1M.poll(g1T, "f1")
      //g2M.poll(g2T, "f2")
      //edgePeriod.roundTo(0.1).poll(isEdge)
      val emitZero = isLongEdge
      val firstShortEdge0 = LocalIn.kr(0)
      val emitOne = isShortEdge & firstShortEdge0
      val firstShortEdge1 = Latch.kr(isShortEdge & !emitOne, isEdge)
      LocalOut.kr(firstShortEdge1)

      //firstShortEdge1.poll(emitOne, "firstShortEdge1")
      //val emitBit = emitZero | emitOne

      (0: GE).poll(emitZero, "BIT")
      (1: GE).poll(emitOne , "BIT")

//      Action(emitZero, "zero")
//      Action(emitOne , "one" )
    }

    val syn = Synth.play(g, nameHint = Some("bi-dec"))(s,
      args = Seq("mic-amp" -> config.decMicAmp),
      dependencies = /*bitBuf ::*/ Nil)
//    syn.onEndTxn { implicit tx =>
//      bitBuf.dispose()
//    }
  }
}

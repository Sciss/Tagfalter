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

import de.sciss.log.{Level, Logger}
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.synth.{InMemory, Server}
import de.sciss.lucre.{Disposable, DoubleVector, InTxnRandom, Random, Workspace}
import de.sciss.numbers.Implicits.doubleNumberWrapper
import de.sciss.proc.{AuralSystem, Proc, Runner, SoundProcesses, TimeRef, Universe}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.{Client, SynthGraph}
import de.sciss.tagfalter.Biphase.{f1a, f1b, f2a, f2b}
import de.sciss.tagfalter.OscNode.DEFAULT_PORT
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import scala.collection.Seq
import scala.concurrent.stm.{InTxn, Ref}

object Main {
  type T = InMemory.Txn

  final val SR = 48000

  case class ConfigImpl(
                         // --- Main ---
                         initCrypMinDur : Float   =  60.0f,
                         initCrypMaxDur : Float   = 180.0f,
                         verbose        : Boolean = false,
                         debug          : Boolean = false,
                         // --- OscNode ---
                         dumpOsc        : Boolean = false,
                         oscPort        : Int     = DEFAULT_PORT,
                         useIp          : Boolean = false,
                         // --- DetectSpace ---
                         unknownLatency : Int     = -64, // frames
                         spaceCorrection: Float   = -26f, // cm
                         minSpacePos    : Int     =  6,
                         maxSpacePos    : Int     = 24,
                         // --- Crypsis ---
                         debugRec       : Boolean =  false,
                         crypMicAmp     : Float   = 40.0f, // 4.0f,
                         crypSpeakerAmp : Float   = 12.0f,
                         cmpThreshIn    : Float   =  10f,
                         cmpThreshOut   : Float   =  15f,
                         crypAchilles   : Float   =  0.98f,
                         crypModFreq    : Float   =  0.5f, // 5.6f,
                         // --- Biphase ---
                         bitPeriod      : Float   = 120.0f,
                         encAmp         : Float   = -20f,
                         decAmp2        : Float   = -10f, // 0.5f,
                         decMicAmp      : Float   = 30f,  // 4.0f,
                         wlanIf         : String  = "wlan0",
                         biphaseF1a     : Float    = f1a,
                         biphaseF1b     : Float    = f1b,
                         biphaseF2a     : Float    = f2a,
                         biphaseF2b     : Float    = f2b,

                       ) extends Config
    with OscNode.Config with DetectSpace.Config with Crypsis.Config with Biphase.Config

  trait Config {
    def initCrypMinDur  : Float
    def initCrypMaxDur  : Float
    def verbose         : Boolean
    def debug           : Boolean
  }

  type ConfigAll = Config with OscNode.Config with DetectSpace.Config with Crypsis.Config with Biphase.Config

  val log: Logger = new Logger("tag")

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {
      printedName = "Tagfalter - Main"
      private val default = ConfigImpl()

      // --- Main ---
      val verbose: Opt[Boolean] = toggle(default = Some(default.verbose),
        descrYes = "Verbosity printing",
      )
      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Debug printing",
      )
      val initCrypMinDur: Opt[Float] = opt(default = Some(default.initCrypMinDur),
        descr = s"Initial minimum crypsis duration in seconds (default: ${default.initCrypMinDur}).",
      )
      val initCrypMaxDur: Opt[Float] = opt(default = Some(default.initCrypMaxDur),
        descr = s"Initial maximum crypsis duration in seconds (default: ${default.initCrypMaxDur}).",
      )

      // --- OscNode ---
      val dumpOsc: Opt[Boolean] = toggle(default = Some(default.dumpOsc),
        descrYes = "Dump incoming OSC messages to console",
      )
      val oscPort: Opt[Int] = opt(default = Some(default.oscPort),
        descr = s"OSC UDP port, -1 to turn off (default: ${default.oscPort}).",
      )
      val useIp: Opt[Boolean] = toggle(default = Some(default.useIp),
        descrYes = "Receive OSC on network IP address instead of loopback",
      )

      // --- Crypsis ---
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

      // --- Biphase ---
      val bitPeriod: Opt[Float] = opt(default = Some(default.bitPeriod),
        descr = s"Bit encoding period in milliseconds (default: ${default.bitPeriod}).",
      )
      val encAmp: Opt[Float] = opt(default = Some(default.encAmp),
        descr = s"Bit encoding amplitude, decibels (default: ${default.encAmp}).",
      )
      val decAmp2: Opt[Float] = opt(default = Some(default.decAmp2),
        descr = s"Bit decoding amplitude for second frequency, decibels (default: ${default.decAmp2}).",
      )
      val decMicAmp: Opt[Float] = opt(default = Some(default.decMicAmp),
        descr = s"Bit decoding microphone boost, decibels (default: ${default.decMicAmp}).",
      )
      val wlanIf: Opt[String] = opt(default = Some(default.wlanIf),
        descr = s"WLAN interface (default: ${default.wlanIf}).",
      )
      val biphaseF1a: Opt[Float] = opt(default = Some(default.biphaseF1a),
        descr = s"Codec frequency 1a, linear (default: ${default.biphaseF1a}).",
      )
      val biphaseF1b: Opt[Float] = opt(default = Some(default.biphaseF1b),
        descr = s"Codec frequency 1b, linear (default: ${default.biphaseF1b}).",
      )
      val biphaseF2a: Opt[Float] = opt(default = Some(default.biphaseF2a),
        descr = s"Codec frequency 2a, linear (default: ${default.biphaseF2a}).",
      )
      val biphaseF2b: Opt[Float] = opt(default = Some(default.biphaseF2b),
        descr = s"Codec frequency 2b, linear (default: ${default.biphaseF2b}).",
      )

      verify()
      implicit val config: ConfigAll = ConfigImpl(
        // --- Main ---
        verbose         = verbose(),
        debug           = debug(),
        initCrypMinDur  = initCrypMinDur(),
        initCrypMaxDur  = initCrypMaxDur(),
        // --- OscNode ---
        dumpOsc         = dumpOsc(),
        oscPort         = oscPort(),
        useIp           = useIp(),
        // --- Crypsis ---
        crypMicAmp      = crypMicAmp(),
        crypSpeakerAmp  = crypSpeakerAmp(),
        cmpThreshIn     = cmpThreshIn(),
        cmpThreshOut    = cmpThreshOut(),
        crypAchilles    = crypAchilles(),
        crypModFreq     = crypModFreq(),
        // --- Biphase ---
        bitPeriod       = bitPeriod(),
        encAmp          = encAmp(),
        decAmp2         = decAmp2(),
        decMicAmp       = decMicAmp(),
        wlanIf          = wlanIf(),
        biphaseF1a      = biphaseF1a(),
        biphaseF1b      = biphaseF1b(),
        biphaseF2a      = biphaseF2a(),
        biphaseF2b      = biphaseF2b(),
      )
    }
    import p.config
    run()
  }

  def detectSpace()(implicit tx: T, config: ConfigAll, universe: Universe[T]): Unit = {
//    implicit val cfgBiphase = Biphase.ConfigImpl()
    Biphase.send(Array(Biphase.CMD_HOLD_ON)) { implicit tx =>
      log.info("Biphase send finished.")
      // implicit val cfgDetect = DetectSpace.ConfigImpl()
      DetectSpace() { implicit tx => resSpace =>
        log.info("DetectSpace finished.")
        // log.info(resSpace.posCm.mkString("Pos [cm]: ", ", ", ""))
        spaceTimbre(resSpace.posCm)
      }
    }
  }

//  def crypsis()(done: T => Unit)(implicit tx: T, config: ConfigAll, universe: Universe[T], random: Random[InTxn]): Unit = {
//    // implicit val cfgCryp: Crypsis.Config = Crypsis.ConfigImpl()
//    val cryp = Crypsis()
//    /*val lCryp: Disposable[T] =*/ cryp.runner.reactNow { implicit tx => state =>
//      if (state.idle) {
//        // lCryp.dispose()
//        cryp.runner.dispose()
//        log.info("Crypsis finished.")
//        done(tx) // detectSpace()
//      }
//    }
//
//    // implicit val cfgBiphase = Biphase.ConfigImpl()
//
//    val sch         = universe.scheduler
//    val tokenRef    = Ref(-1)
//    val schTimeRef  = Ref(0L)
//    val rcvRef      = Ref(Disposable.empty[T])
//
//    def schedule(minDur: Double, maxDur: Double)(implicit tx: T): Unit = {
//      val dlySec  = random.nextDouble().linLin(0.0, 1.0, minDur, maxDur)
//      log.info((f"Crypsis duration in seconds: $dlySec%1.1f"))
//      val dlyFr   = (dlySec * TimeRef.SampleRate).toLong
//      val schTime = sch.time + dlyFr
//      val token = sch.schedule(schTime) { implicit tx =>
//        log.debug("Releasing crypsis (and biphase rcv)")
//        cryp.release()
//        rcvRef().dispose()
//      }
//      schTimeRef() = schTime
//      sch.cancel(tokenRef.swap(token))
//    }
//
//    schedule(minDur = config.initCrypMinDur, maxDur = config.initCrypMaxDur)
//
//    val rcv = Biphase.receive() { implicit tx => byte =>
//      // if (byte == Biphase.CMD_HOLD_ON)
//      log.info("Received global communication")
//      val dlyGlobSec  = 10.0
//      val dlyGlobFr   = (dlyGlobSec * TimeRef.SampleRate).toLong
//      val timeMin     = sch.time + dlyGlobFr
//      val schTime     = schTimeRef()
//      if (schTime < timeMin) {
//        log.info("Have to reschedule crypsis")
//        // important to schedule further than `dlyGlobSec` because transmission could call
//        // `consume` again now.
//        val durGlobSec = 5.0
//        schedule(minDur = dlyGlobSec + durGlobSec, maxDur = dlyGlobSec * 2 + durGlobSec)
//      }
//    }
//    rcvRef() = rcv
//
//    //    rcv.dispose()
//
//  }

  def run()(implicit config: ConfigAll): Unit = {
    if (config.verbose) log.level = Level.Info
    if (config.debug  ) log.level = Level.Debug

    boot { implicit tx => implicit universe => _ /*s*/ =>
      /*val m: Machine =*/ Machine()
//      implicit val random: Random[InTxn] = InTxnRandom()
//      crypsis() { implicit tx =>
//        detectSpace()
//      }

//      implicit val cfgDetect: DetectSpace.Config = DetectSpace.ConfigImpl()
//      DetectSpace() { implicit tx => res =>
//        spaceTimbre(res.posCm)
//      }

      if (config.oscPort != -1) tx.afterCommit {
        OscNode.run()
      }
    }
  }

  def spaceTimbre(vec: Vec[Float])(implicit tx: T, config: Config, universe: Universe[T]): Unit = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      val space     = "space".kr(1.0) // Seq(1.0, 2.0))
      //space.poll(0, "space")
      val spaceLo   = Reduce.min(space)
      val spaceHi   = Reduce.max(space)
      if (config.debug) spaceLo.poll(0, "space-lo")
      if (config.debug) spaceHi.poll(0, "space-hi")
      val spaceMin  =    60.0 // 10.0
      val spaceMax  = 12000.0 // 2000.0
      val freqMin   =   150.0 // 250.0 // 150.0
      val freqMax   = 15000.0
      val freqSeq   = space.clip(spaceMin, spaceMax)
        .linExp(spaceMin, spaceMax, freqMin, freqMax)
      //val freqSeq   = space.clip(spaceMin, spaceMax)
      //  .linLin(spaceMin, spaceMax, freqMin, freqMax)
      val oscAmpSeq = Vec.tabulate(vec.size) { i =>
        LFNoise1.kr(i.linLin(0, vec.size - 1, 0.11, 0.23)).abs
      }
      val oscSeq    = SinOsc.ar(freqSeq) * oscAmpSeq
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

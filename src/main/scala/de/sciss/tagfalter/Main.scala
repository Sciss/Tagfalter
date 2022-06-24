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
import de.sciss.lucre.Workspace
import de.sciss.lucre.synth.{InMemory, Server}
import de.sciss.proc.{AuralSystem, SoundProcesses, Universe}
import de.sciss.synth.{Client, Curve}
import de.sciss.tagfalter.Biphase.{f1a, f1b, f2a, f2b}
import de.sciss.tagfalter.OscNode.DEFAULT_PORT
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.File
import scala.collection.Seq

object Main {
  type T = InMemory.Txn

  final val SR = 48000

  case class ConfigImpl(
                         // --- Main ---
                         initCrypMinDur : Float   =  60.0f,
                         initCrypMaxDur : Float   = 180.0f,
                         initAccMinDur  : Float   =  60.0f,
                         initAccMaxDur  : Float   = 180.0f,
                         verbose        : Boolean = true,
                         debug          : Boolean = false,
                         accelMinFactor : Float   = 16f,
                         accelMaxFactor : Float   = 64f,
                         accelRecTime   : Float   = 600f,
                         spaceMinDur    : Float   = 32f,
                         spaceMaxDur    : Float   = 80f,
                         crypModMinFreq : Float   =  0.07f,
                         crypModMaxFreq : Float   =  0.25f,
                         spaceAmpMaxDamp: Float   = 6.0f, // decibels
                         detectSpacePeriod : Float  = 600f, // 480f, // in seconds
                         nodeId         : Int     = -1,
                         silenceProb    : Float   = 0.15f,
                         silenceMinDur  : Float   = 30f,
                         silenceMaxDur  : Float   = 90f,
                         commMinFreq    : Float   =   700f,
                         commMaxFreq    : Float   = 18000f,
                         joyProb        : Float   = 0.1f,
                         dirAudio       : File    = new File("audio_work"),
                         encAmpComm     : Float   = -23f, // -20f,
                         rebootTimeOut  : Int     = 600,  // 10 minutes
                         // --- Silence ---
                         silenceAmp     : Float   = -24f, // -30f,
                         silenceFreq    : Float   = 34f,
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
                         crypSpeakerAmp : Float   = 15.0f, // 12.0f,
                         cmpThreshIn    : Float   =  10f,
                         cmpThreshOut   : Float   =  15f,
                         crypAchilles   : Float   =  0.98f,
                         crypModFreq    : Float   =  0.15f, // 0.25f, // 0.5f, // 5.6f,
                         // --- Biphase ---
                         bitPeriod      : Float   = 120.0f,
                         encAmp         : Float   = -24f, // -20f,
                         decAmp2        : Float   = -10f, // 0.5f,
                         decMicAmp      : Float   = 30f, // 4.0f,
                         wlanIf         : String  = "wlan0",
                         biphaseF1a     : Float    = f1a,
                         biphaseF1b     : Float    = f1b,
                         biphaseF2a     : Float    = f2a,
                         biphaseF2b     : Float    = f2b,
                         // --- SpaceTimbre ---
                         spaceMinCm     : Float   =    60.0f, // 10.0
                         spaceMaxCm     : Float   = 12000.0f, // 2000.0
                         spaceMinFreq   : Float   =   200.0f, // 150.0f,
                         spaceMaxFreq   : Float   = 24000f, // 18000.0f,
                         spaceAmp       : Float   = -10.0f, // decibels
                         spaceCurve: Curve = Curve.squared,
                         // --- Accelerate ---
                         accelMicAmp    : Float   = 10.0f,
                         accelSigAmp    : Float   =  2.0f, // 1.0f,
                         accelCmpThresh : Float   =  15f,
                       ) extends Config
    with OscNode      .Config
    with DetectSpace  .Config
    with Crypsis      .Config
    with Biphase      .Config
    with SpaceTimbre  .Config
    with Accelerate   .Config
    with Silence      .Config

  trait Config {
    def initCrypMinDur    : Float
    def initCrypMaxDur    : Float
    def initAccMinDur     : Float
    def initAccMaxDur     : Float
    def verbose           : Boolean
    def debug             : Boolean
    def accelMinFactor    : Float
    def accelMaxFactor    : Float
    def accelRecTime      : Float
    def spaceMinDur       : Float
    def spaceMaxDur       : Float
    def crypModMinFreq    : Float
    def crypModMaxFreq    : Float
    def spaceAmpMaxDamp   : Float
    def detectSpacePeriod : Float
    def nodeId            : Int
    def silenceProb       : Float
    def silenceMinDur     : Float
    def silenceMaxDur     : Float
    def silenceAmp        : Float
    def silenceFreq       : Float
    def commMinFreq       : Float
    def commMaxFreq       : Float
    def joyProb           : Float
    def dirAudio          : File
    /** Decibels */
    def encAmpComm        : Float
    def rebootTimeOut     : Int
  }

  type ConfigAll = Config
    with OscNode    .Config
    with DetectSpace.Config
    with Crypsis    .Config
    with Biphase    .Config
    with SpaceTimbre.Config
    with Accelerate .Config
    with Silence    .Config

  val log: Logger = new Logger("tag")

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {
      printedName = "Tagfalter - Main"
      private val default = ConfigImpl()

      import SpaceTimbre.ReadCurve

      // --- Main ---
      val verbose: Opt[Boolean] = toggle(default = Some(default.verbose),
        descrYes = "Verbosity printing (default)",
        descrNo  = "Disable verbosity printing",
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
      val initAccMinDur: Opt[Float] = opt(default = Some(default.initAccMinDur),
        descr = s"Initial minimum acceleration play duration in seconds (default: ${default.initAccMinDur}).",
      )
      val initAccMaxDur: Opt[Float] = opt(default = Some(default.initAccMaxDur),
        descr = s"Initial maximum acceleration play duration in seconds (default: ${default.initAccMaxDur}).",
      )
      val accelMinFactor: Opt[Float] = opt(default = Some(default.accelMinFactor),
        descr = s"Minimum acceleration factor (default: ${default.accelMinFactor}).",
      )
      val accelMaxFactor: Opt[Float] = opt(default = Some(default.accelMaxFactor),
        descr = s"Maximum acceleration factor (default: ${default.accelMaxFactor}).",
      )
      val accelRecTime: Opt[Float] = opt(default = Some(default.accelRecTime),
        descr = s"Acceleration recording time per buffer, in seconds (default: ${default.accelRecTime}).",
      )
      val spaceMinDur: Opt[Float] = opt(default = Some(default.spaceMinDur),
        descr = s"Space-timbre minimum duration, in seconds (default: ${default.spaceMinDur}).",
      )
      val spaceMaxDur: Opt[Float] = opt(default = Some(default.spaceMaxDur),
        descr = s"Space-timbre maximum duration, in seconds (default: ${default.spaceMaxDur}).",
      )
      val crypModMinFreq: Opt[Float] = opt(default = Some(default.crypModMinFreq),
        descr = s"Crypsis minimum modulation frequency in Hz (default: ${default.crypModMinFreq}).",
      )
      val crypModMaxFreq: Opt[Float] = opt(default = Some(default.crypModMaxFreq),
        descr = s"Crypsis minimum modulation frequency in Hz (default: ${default.crypModMaxFreq}).",
      )
      val spaceAmpMaxDamp: Opt[Float] = opt(default = Some(default.spaceAmpMaxDamp),
        descr = s"Space-timbre maximum amplitude damping, in decibels (default: ${default.spaceAmpMaxDamp}).",
      )
      val detectSpacePeriod: Opt[Float] = opt(default = Some(default.detectSpacePeriod),
        descr = s"Minimum period between two runs of detect-space, in seconds (default: ${default.detectSpacePeriod}).",
        validate = x => x > 0.0f
      )
      val nodeId: Opt[Int] = opt(
        descr = "Node-id or -1 (default) to automatically detect via 'uname'.",
      )
      val silenceProb: Opt[Float] = opt(default = Some(default.silenceProb),
        descr = s"Silent stage probability 0 to 1 (default: ${default.silenceProb}).",
        validate = x => x >= 0f && x <= 1f
      )
      val silenceMinDur: Opt[Float] = opt(default = Some(default.silenceMinDur),
        descr = s"Silent minimum duration, in seconds (default: ${default.silenceMinDur}).",
        validate = x => x > 0f
      )
      val silenceMaxDur: Opt[Float] = opt(default = Some(default.silenceMaxDur),
        descr = s"Silent maximum duration, in seconds (default: ${default.silenceMaxDur}).",
        validate = x => x > 0f
      )
      val commMinFreq: Opt[Float] = opt(default = Some(default.commMinFreq),
        descr = s"Minimum space-id communication frequency in Hz (default: ${default.commMinFreq}).",
      )
      val commMaxFreq: Opt[Float] = opt(default = Some(default.commMaxFreq),
        descr = s"Maximum space-id communication frequency in Hz (default: ${default.commMaxFreq}).",
      )
      val joyProb: Opt[Float] = opt(default = Some(default.joyProb),
        descr = s"Joy stage probability 0 to 1 (default: ${default.joyProb}).",
        validate = x => x >= 0f && x <= 1f
      )
      val dirAudio: Opt[File] = opt(default = Some(default.dirAudio),
        descr = s"Audio file directory (default: ${default.dirAudio})"
      )
      val encAmpComm: Opt[Float] = opt(default = Some(default.encAmpComm),
        descr = s"Bit encoding amplitude (individual communication), decibels (default: ${default.encAmpComm}).",
      )
      val rebootTimeOut: Opt[Int] = opt(default = Some(default.rebootTimeOut),
        descr = s"Idle reboot time-out in seconds (default: ${default.rebootTimeOut}).",
      )

      // --- Silence ---
      val silenceAmp: Opt[Float] = opt(default = Some(default.silenceAmp),
        descr = s"Silent click amp, in decibels (default: ${default.silenceAmp}).",
      )
      val silenceFreq: Opt[Float] = opt(default = Some(default.silenceFreq),
        descr = s"Silent click frequency, in Hz (default: ${default.silenceFreq}).",
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
        descr = s"Bit encoding amplitude (global), decibels (default: ${default.encAmp}).",
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

      // --- SpaceTimbre ---
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

      // --- Accelerate ---
      val accelMicAmp: Opt[Float] = opt(default = Some(default.accelMicAmp),
        descr = s"Acceleration microphone boost, linear (default: ${default.accelMicAmp}).",
      )
      val accelSigAmp: Opt[Float] = opt(default = Some(default.accelSigAmp),
        descr = s"Acceleration signal boost, linear (default: ${default.accelSigAmp}).",
      )
      val accelCmpThresh: Opt[Float] = opt(default = Some(default.accelCmpThresh),
        descr = s"Accelerate output compression threshold in neg.decibels (default: ${default.accelCmpThresh}).",
      )

      verify()
      implicit val config: ConfigAll = ConfigImpl(
        // --- Main ---
        verbose           = verbose(),
        debug             = debug(),
        initCrypMinDur    = initCrypMinDur(),
        initCrypMaxDur    = initCrypMaxDur(),
        initAccMinDur     = initAccMinDur(),
        initAccMaxDur     = initAccMaxDur(),
        accelMinFactor    = accelMinFactor(),
        accelMaxFactor    = accelMaxFactor(),
        accelRecTime      = accelRecTime(),
        spaceMinDur       = spaceMinDur(),
        spaceMaxDur       = spaceMaxDur(),
        crypModMinFreq    = crypModMinFreq(),
        crypModMaxFreq    = crypModMaxFreq(),
        spaceAmpMaxDamp   = spaceAmpMaxDamp(),
        detectSpacePeriod = detectSpacePeriod(),
        nodeId            = nodeId.getOrElse(autoNodeId()),
        silenceProb       = silenceProb(),
        silenceMinDur     = silenceMinDur(),
        silenceMaxDur     = silenceMaxDur(),
        commMinFreq       = commMinFreq(),
        commMaxFreq       = commMaxFreq(),
        joyProb           = joyProb(),
        dirAudio          = dirAudio().getAbsoluteFile,
        encAmpComm        = encAmpComm(),
        rebootTimeOut     = rebootTimeOut(),
        // --- Silence ---
        silenceAmp        = silenceAmp(),
        silenceFreq       = silenceFreq(),
        // --- OscNode ---
        dumpOsc           = dumpOsc(),
        oscPort           = oscPort(),
        useIp             = useIp(),
        // --- Crypsis ---
        crypMicAmp        = crypMicAmp(),
        crypSpeakerAmp    = crypSpeakerAmp(),
        cmpThreshIn       = cmpThreshIn(),
        cmpThreshOut      = cmpThreshOut(),
        crypAchilles      = crypAchilles(),
        crypModFreq       = crypModFreq(),
        // --- Biphase ---
        bitPeriod         = bitPeriod(),
        encAmp            = encAmp(),
        decAmp2           = decAmp2(),
        decMicAmp         = decMicAmp(),
        wlanIf            = wlanIf(),
        biphaseF1a        = biphaseF1a(),
        biphaseF1b        = biphaseF1b(),
        biphaseF2a        = biphaseF2a(),
        biphaseF2b        = biphaseF2b(),
        // --- SpaceTimbre ---
        spaceMinCm        = spaceMinCm(),
        spaceMaxCm        = spaceMaxCm(),
        spaceMinFreq      = spaceMinFreq(),
        spaceMaxFreq      = spaceMaxFreq(),
        spaceAmp          = spaceAmp(),
        spaceCurve        = spaceCurve(),
        // --- Accelerate ---
        accelMicAmp       = accelMicAmp(),
        accelSigAmp       = accelSigAmp(),
        accelCmpThresh    = accelCmpThresh(),
      )
    }
    import p.config
    run()
  }

  def autoNodeId(): Int = {
    import sys.process._
    val nodeName = Seq("uname", "-n").!!.trim
    if (nodeName == "aleph") 0 else if (nodeName.startsWith("klangpi")) {
      nodeName.substring("klangpi".length).toInt % 32
    } else {
      Console.err.println(s"WARNING: Cannot parse node-name $nodeName. Falling back to node-id 0")
      0
    }
  }

  def run()(implicit config: ConfigAll): Unit = {
    if (config.verbose) log.level = Level.Info
    if (config.debug  ) log.level = Level.Debug

    log.info(s"Node-id is ${config.nodeId}")

    boot { implicit tx => implicit universe => _ /*s*/ =>
      Machine()
      if (config.oscPort != -1) tx.afterCommit {
        OscNode.run()
      }
    }
  }

  def printInfo(): Unit = {
    import BuildInfo._
    println(s"$name v$version, built $builtAtString")
  }

  def boot(done: T => Universe[T] => Server => Unit): Unit = {
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
            // s.peer.dumpOSC()
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

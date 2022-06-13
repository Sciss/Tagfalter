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
import de.sciss.synth.Client
import de.sciss.tagfalter.Biphase.{f1a, f1b, f2a, f2b}
import de.sciss.tagfalter.OscNode.DEFAULT_PORT
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import scala.collection.Seq

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
                         // --- SpaceTimbre ---
                         spaceMinCm    : Float   =    60.0f, // 10.0
                         spaceMaxCm    : Float   = 12000.0f, // 2000.0
                         spaceMinFreq  : Float   =   150.0f,
                         spaceMaxFreq  : Float   = 18000.0f,
                         spaceAmp      : Float   = 0.0f, // decibels
                       ) extends Config
    with OscNode.Config with DetectSpace.Config with Crypsis.Config with Biphase.Config with SpaceTimbre.Config

  trait Config {
    def initCrypMinDur  : Float
    def initCrypMaxDur  : Float
    def verbose         : Boolean
    def debug           : Boolean
  }

  type ConfigAll = Config
    with OscNode    .Config
    with DetectSpace.Config
    with Crypsis    .Config
    with Biphase    .Config
    with SpaceTimbre.Config

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
        // --- SpaceTimbre ---
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: ConfigAll): Unit = {
    if (config.verbose) log.level = Level.Info
    if (config.debug  ) log.level = Level.Debug

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

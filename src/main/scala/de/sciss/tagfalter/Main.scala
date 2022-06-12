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
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import scala.collection.Seq
import scala.concurrent.stm.{InTxn, Ref}

object Main {
  type T = InMemory.Txn

  final val SR = 48000

  case class ConfigImpl(
                         initCrypMinDur : Float   =  60.0f,
                         initCrypMaxDur : Float   = 180.0f,
                         verbose        : Boolean = false,
                         debug          : Boolean = false,
                       ) extends Config

  trait Config {
    def initCrypMinDur  : Float
    def initCrypMaxDur  : Float
    def verbose         : Boolean
    def debug           : Boolean
  }

  val log: Logger = new Logger("tag")

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {
      printedName = "Tagfalter - Main"
      private val default = ConfigImpl()

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

      verify()
      implicit val config: Config = ConfigImpl(
        verbose         = verbose(),
        debug           = debug(),
        initCrypMinDur  = initCrypMinDur(),
        initCrypMaxDur  = initCrypMaxDur(),
      )
    }
    import p.config
    run()
  }

  def detectSpace()(implicit tx: T, config: Config, universe: Universe[T]): Unit = {
    implicit val cfgBiphase = Biphase.ConfigImpl()
    Biphase.send(Array(Biphase.CMD_HOLD_ON)) { implicit tx =>
      log.info("Biphase send finished.")
      implicit val cfgDetect = DetectSpace.ConfigImpl()
      DetectSpace() { implicit tx => resSpace =>
        log.info("DetectSpace finished.")
        // log.info(resSpace.posCm.mkString("Pos [cm]: ", ", ", ""))
        spaceTimbre(resSpace.posCm)
      }
    }
  }

  def crypsis()(done: T => Unit)(implicit tx: T, config: Config, universe: Universe[T], random: Random[InTxn]): Unit = {
    implicit val cfgCryp: Crypsis.Config = Crypsis.ConfigImpl()
    val cryp = Crypsis()
    /*val lCryp: Disposable[T] =*/ cryp.runner.reactNow { implicit tx => state =>
      if (state.idle) {
        // lCryp.dispose()
        cryp.runner.dispose()
        log.info("Crypsis finished.")
        done(tx) // detectSpace()
      }
    }

    implicit val cfgBiphase = Biphase.ConfigImpl()

    val sch         = universe.scheduler
    val tokenRef    = Ref(-1)
    val schTimeRef  = Ref(0L)
    val rcvRef      = Ref(Disposable.empty[T])

    def schedule(minDur: Double, maxDur: Double)(implicit tx: T): Unit = {
      val dlySec  = random.nextDouble().linLin(0.0, 1.0, minDur, maxDur)
      log.info((f"Crypsis duration in seconds: $dlySec%1.1f"))
      val dlyFr   = (dlySec * TimeRef.SampleRate).toLong
      val schTime = sch.time + dlyFr
      val token = sch.schedule(schTime) { implicit tx =>
        log.debug("Releasing crypsis (and biphase rcv)")
        cryp.release()
        rcvRef().dispose()
      }
      schTimeRef() = schTime
      sch.cancel(tokenRef.swap(token))
    }

    schedule(minDur = config.initCrypMinDur, maxDur = config.initCrypMaxDur)

    val rcv = Biphase.receive() { implicit tx => byte =>
      // if (byte == Biphase.CMD_HOLD_ON)
      log.info("Received global communication")
      val dlyGlobSec  = 10.0
      val dlyGlobFr   = (dlyGlobSec * TimeRef.SampleRate).toLong
      val timeMin     = sch.time + dlyGlobFr
      val schTime     = schTimeRef()
      if (schTime < timeMin) {
        log.info("Have to reschedule crypsis")
        // important to schedule further than `dlyGlobSec` because transmission could call
        // `consume` again now.
        val durGlobSec = 5.0
        schedule(minDur = dlyGlobSec + durGlobSec, maxDur = dlyGlobSec * 2 + durGlobSec)
      }
    }
    rcvRef() = rcv

    //    rcv.dispose()

  }

  def run()(implicit config: Config): Unit = {
    if (config.verbose) log.level = Level.Info
    if (config.debug  ) log.level = Level.Debug

    boot { implicit tx => implicit universe => _ /*s*/ =>
      implicit val random: Random[InTxn] = InTxnRandom()
      crypsis() { implicit tx =>
        detectSpace()
      }

//      implicit val cfgDetect: DetectSpace.Config = DetectSpace.ConfigImpl()
//      DetectSpace() { implicit tx => res =>
//        spaceTimbre(res.posCm)
//      }
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

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

import de.sciss.audiofile.{AudioFile, AudioFileSpec}
import de.sciss.file.userHome
import de.sciss.lucre.Txn.{peer => peerTx}
import de.sciss.lucre.synth.{InMemory, Server, Synth}
import de.sciss.lucre.{Artifact, ArtifactLocation, DoubleVector, Workspace}
import de.sciss.proc.{AudioCue, AuralSystem, Proc, Runner, SoundProcesses, TimeRef, Universe}
import de.sciss.rogues.BuildInfo
import de.sciss.synth.{Client, SynthGraph}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.File
import scala.concurrent.stm.Ref

object Main {
  type T = InMemory.Txn

  final val SR = 48000

  case class Config(
                   debug: Boolean = false
                   )

  def main(args: Array[String]): Unit = {
    {
      import BuildInfo._
      println(s"$name v$version, built $builtAtString")
    }

    object p extends ScallopConf(args) {

      import org.rogach.scallop._

      printedName = "Tagfalter"
      private val default = Config()

      //      val numCircles: Opt[Int] = opt(default = Some(default.numCircles),
      //        descr = s"Number of moons, 3 or larger (default: ${default.numCircles}).",
      //        validate = x => x >= 3
      //      )
      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )

      verify()
      implicit val config: Config = Config(
        debug = debug(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
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
            SoundProcesses.step[T]("detect-space") { implicit tx =>
              implicit val ws: Workspace[T] = Workspace.Implicits.dummy[T]
              implicit val u: Universe[T] = Universe[T]()
              detectSpace(s)
            }
          }

        case _ => ()
      }}
      as.start(sCfg, cCfg)
    }
  }

  def detectSpace(s: Server)(implicit tx: T, universe: Universe[T], config: Config): Unit = {
    // the Pi HAT sound card is the worst piece of crap
    // I have seen in years. It "pauses" the alsa driver, or something like that,
    // when it sees a zero signal. When it "resumes", we get random latencies.
    // To avoid that, we add a permanent noise output.
    println("Start background noise")
    val gNoise = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      val sig = WhiteNoise.ar(4.0e-4) // -68 dB -- minimum volume necessary to unblock audio driver
      if (config.debug) sig.poll(0, "NOISE")
      PhysicalOut.ar(0, sig)
    }
    /*val synNoise =*/ Synth.play(gNoise, nameHint = Some("noise"))(s)

    import universe.scheduler
    val now = scheduler.time
    scheduler.schedule(now + (TimeRef.SampleRate * 2).toLong) { implicit tx =>
      println("Start detect space")
      detectSpaceImpl()
    }
  }

  private def detectSpaceImpl()(implicit tx: T, universe: Universe[T], config: Config): Unit = {
    val p = Proc[T]()
    val dirAudio  = new File(userHome, "Documents/projects/Klangnetze/audio_work")
    val locAudio  = ArtifactLocation.newConst[T](dirAudio.toURI)
    val artFwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHzPre.aif"))
    val artBwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHzRvs.aif"))
    val specFwd   = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 3 * SR)
    val specBwd   = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 2 * SR)
    val cueFwd    = AudioCue.Obj[T](artFwd, specFwd, 0L, 1.0)
    val cueBwd    = AudioCue.Obj[T](artBwd, specBwd, 0L, 1.0)

    val NumRuns   = 4
    val RunIdx    = Ref(0)

    p.graph() = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
       val sweep    = DiskIn.ar("in")
      val gainSpkr  = 0.2 // "gain-in" .kr(0.2)
      val gainMic   = 1.0 // 4.0 // "gain-out".kr(4.0)
      val outChan   = 0 // "out-ch"    .kr(0)
      val durTotal  = 3.0 // "total-dur-sec".kr(3)
      val sigOut    = sweep * gainSpkr
      PhysicalOut.ar(outChan, sigOut)
      val sigIn     = PhysicalIn.ar * gainMic
      if (config.debug) sigIn.poll(0, "MIC")
      //DiskOut.ar("out", sigIn)
      val fftSize  = 2048
      val deConv   = PartConv.ar("sweep-rvs", sigIn, fftSize = fftSize)
      val framesTotal = durTotal * SR
      val buf     = Buffer.Empty(framesTotal)
      val PRE_DELAY  = (3 * SR).toInt   // damn Pi chokes if we don't add a pre-delay
      val JACK_BLOCK_SIZE = 1024
      val JACK_NUM_BLOCKS = 3
      // there is a crazy variance in the latency between scsynth/jackd, up to several hundred sample frames.
      // no idea how to explain it. This one works in 80-90% of the cases
      val UNKNOWN_DELAY = -1024 // -1024 //  -512 // 448 // -128 // 0 // 256 // 320
      val CONV_DELAY    = JACK_BLOCK_SIZE * JACK_NUM_BLOCKS /*+ fftSize*/ + UNKNOWN_DELAY
      val recRun        = ToggleFF.kr(TDelay.kr(Impulse.kr(0), (PRE_DELAY + CONV_DELAY).toDouble / SR))
      val r             = RecordBuf.ar(deConv /*sigIn*/, buf, loop = 0, run = recRun)
      val recDone       = Done.kr(r)
      //StopSelf(Done.kr(elapsed))
      val getDone = Action.GetBuf(recDone, "out", buf)
      DoneSelf(getDone)
    }

    val vrSweep = DoubleVector.newVar[T](Vector.empty)
    val pAttr = p.attr
    pAttr.put("in", cueFwd)
    pAttr.put("sweep-rvs", cueBwd)
    pAttr.put("out", vrSweep)
    val r = Runner(p)

    def nextRun()(implicit tx: T): Unit = {
      println(s"Running sweep rec (iteration ${RunIdx()})")
      r.run()
    }

    r.react { implicit tx => {
      case Runner.Done =>
        println("(rec done)")
//        val sweep = vrSweepEx.value
        val sweep     = vrSweep.value
        val normGain  = if (sweep.isEmpty) 1.0 else 1.0 / Math.max(1.0e-6, sweep.iterator.map(x => Math.abs(x)).max)
        val sweepNorm = sweep.map(_ * normGain)
//        val numEmpty = sweep.segmentLength(_ == 0.0)
//        println(s"numEmpty $numEmpty")
//        println(sweepNorm./*drop(numEmpty).*/take(256).mkString(", "))

        // the first one is still badly cropped
        if (config.debug  && RunIdx() > 0) {
          val afOut = AudioFile.openWrite(new File(dirAudio, s"_killme${RunIdx()}.aif"),
            AudioFileSpec(numChannels = 1, sampleRate = SR))
          afOut.write(Array(sweepNorm.toArray))
          afOut.close()
        }

        if (RunIdx.transformAndGet(_ + 1) <= NumRuns) {
          nextRun()
        } else {
          sys.exit()
        }

      case Runner.Running =>
        println("(rec running)")
//        new Thread {
//          override def run(): Unit = {
//            Thread.sleep(2000)
//            val s = de.sciss.synth.Server.default
//            s.dumpOSC(osc.Dump.Off)
//          }
//        } .start()

      case state =>
        println(s"(rec state $state)")
    }}

     nextRun()
  }
}

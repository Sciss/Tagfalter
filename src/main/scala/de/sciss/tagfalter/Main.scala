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
import de.sciss.numbers.Implicits.intNumberWrapper
import de.sciss.proc.{AudioCue, AuralSystem, Proc, Runner, SoundProcesses, TimeRef, Universe}
import de.sciss.rogues.BuildInfo
import de.sciss.synth.{Client, SynthGraph}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.File
import scala.concurrent.stm.Ref

object Main {
  type T = InMemory.Txn

  final val SR = 48000
  private final val durIRRec      = 3.0 // "total-dur-sec".kr(3)
  private final val framesIRRec   = (durIRRec * SR).toInt
  private final val threshExcess  = -30.dbAmp // -40.dbAmp
  private final val rangeLocalMax = 128 // 32
  private final val maxIRPos      = framesIRRec / rangeLocalMax
  private final val speedOfSound  = 343.0   // m/s

  case class Config(
                   debug          : Boolean = false,
                   noise          : Boolean = false,
                   unknownLatency : Int     = -64,
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

      val unknownLatency: Opt[Int] = opt(default = Some(default.unknownLatency),
        descr = s"Add unknown latency in sample frames to IR measurement (default: ${default.unknownLatency}).",
      )
      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )
      val noise: Opt[Boolean] = toggle(default = Some(default.noise),
        descrYes = "Add background noise sound (testing).",
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
    if (config.noise) {
      // Note: this problem has been reduced to the use of DiskIn over Buffer/PlayBuf
      // (SoundProcesses issue #117)
      //
      // (NOT:) the Pi HAT sound card "pauses" the alsa driver, or something like that,
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
    }

    import universe.scheduler
    val now = scheduler.time
    scheduler.schedule(now + (TimeRef.SampleRate * 2).toLong) { implicit tx =>
      println("Start detect space")
      detectSpaceImpl()
    }
  }

  private def normalize(in: Array[Double]): Unit = {
    val len = in.length
    var i = 0
    var max = Math.abs(in(0))
    while (i < len) {
      val x0    = in(i)
      val x0Abs = if (x0 >= 0.0) x0 else -x0
      if (x0Abs > max) max = x0Abs
      i += 1
    }
    if (max > 0.0) {
      val gain = 1.0 / max
      i = 0
      val out = in
      while (i < len) {
        val x0 = in(i)
        val y0 = x0 * gain
        out(i) = y0
        i += 1
      }
    }
  }

  private def prepareIR(in: Array[Double]): Unit = {
    // from mllt: sliding three frames 'max', center clipping, and abs

    val out = in
    val len = in.length
    var i = 0
    var x1 = in(0)
    var x2 = x1
    while (i < len) {
      val x0 = in(i)
      val y0Smooth = if (x0 > x1) {
        if (x0 > x2) x0 else x2
      } else if (x1 > x2) x1 else x2

      val y0Abs = if (y0Smooth >= 0.0) y0Smooth else -y0Smooth
      val y0ExcM = y0Abs - threshExcess
      val y0Exc = if (y0ExcM >= 0.0) y0ExcM else 0.0
      val y0 = y0Exc
      out(i) = y0
      x2 = x1; x1 = x0
      i += 1
    }
  }

  // `out` should have a size of at least `maxIRPos`
  private def detectLocalMax(in: Array[Double], out: Array[Int]): Int = {
    val len = in.length

    var readMode      = true
    var complete      = false

    var state         = 0     // 0 - empty, 1 - found, 2 - blocked
    var foundValue    = 0.0
    var foundFrame    = 0
    var stopFrame     = 0
    // var writtenTrig   = 0
    var x0            = 0.0
    var upwards       = false
    var inRemain      = len
    var framesRead    = 0
    var framesWritten = 0
    val size          = rangeLocalMax

    while (!complete) {
      if (readMode) {
        if (inRemain == 0 /*&& !_complete*/) {
          if (state != 1) {
            foundFrame = -1 // framesRead // place a virtual found frame at the end
          }
          state       = 2 // stay in this mode (we will never switch back to read-mode)
          // debug(s"up-stream terminated. state -> $state")
          readMode    = false
          complete    = true

        } else {
          def calcSkip(): Int = if (state == 0) inRemain else Math.min(inRemain, stopFrame - framesRead)

          var skip      = calcSkip()

          while (skip > 0) {
            val x1      = in(framesRead)
            val up  =         x1 >= x0
            val gt1 = up   && x1 >  x0
            val lt1 = !gt1 && x1 <  x0

            if (state != 2) {
              val isMax = upwards && lt1
              if (isMax) {
                upwards = false
                if (state == 0) {
                  foundFrame  = framesRead - 1

                  // the following two alternative definitions yield
                  // very different outcomes. The first `foundFrame + size`
                  // tends to produce "more sparse" results, but is better
                  // at catching the highest peaks. The second
                  // `max(writtenTrig + size, foundFrame)` is more dense,
                  // but also performs worse in getting the highest peaks.

                  stopFrame   = foundFrame + size + 1
                  foundValue  = x0
                  state       = 1
                  // debug(s"local max $foundFrame; stop-frame $stopFrame; state -> $state")
                } else {  // state == 1
                  // debug(s"local max $framesRead...")
                  if (x0 > foundValue) {
                    foundFrame  = framesRead - 1
                    foundValue  = x0
                    // debug("...is larger")
                  }
                }
              }
            }

            inRemain   -= 1
            framesRead += 1
            x0         = x1
            upwards    = up
            skip        = calcSkip()
          } // while (skip > 0)

          if (state != 0) {
            val reachedStopFrame  = framesRead == stopFrame
            val terminate         = inRemain == 0 // && isClosed(shape.in0) && !isAvailable(shape.in0)
            // debug(s"reachedStopFrame? $reachedStopFrame; terminate? $terminate")
            if (reachedStopFrame || terminate) {
              // go from 'found' to 'blocked', from 'blocked' to 'empty'
              if (terminate) {
                complete   = true
              }
              state = if (state == 1) 2 else 0
              // debug(s"... state -> $state")
              if (state == 2) {
                readMode = false
              }

              // stateChanged = true
            }
          }
        }
      }

      // note: other than FScape, we always check here again,
      // so we do not need to check before completion
      if (!readMode) { // write mode
        if (foundFrame >= 0) {
          out(framesWritten)  = foundFrame
          framesWritten      += 1
          stopFrame           = foundFrame + size + 1
          // writtenTrig         = foundFrame
          readMode            = true  // continue 'blocked' read
        }
      }
    }

    framesWritten
  }

  private def detectSpaceImpl()(implicit tx: T, universe: Universe[T], config: Config): Unit = {
    val p = Proc[T]()
    val dirAudio  = new File(userHome, "Documents/projects/Klangnetze/audio_work")
    val locAudio  = ArtifactLocation.newConst[T](dirAudio.toURI)
    // XXX TODO: go back to version without initial delay
    val artFwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHzPre.aif"))
    val artBwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHzRvs.aif"))
    val specFwd   = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 3 * SR)
    val specBwd   = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 2 * SR)
    val cueFwd    = AudioCue.Obj[T](artFwd, specFwd, 0L, 1.0)
    val cueBwd    = AudioCue.Obj[T](artBwd, specBwd, 0L, 1.0)

    val NumRuns   = 4
    val RunIdx    = Ref(1) // (0)

    p.graph() = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
//      val sweep     = DiskIn.ar("in")
      val sweepBuf  = Buffer("in")
      val sweep     = PlayBuf.ar(1, sweepBuf, loop = 0)
      val gainSpkr  = 0.2 // "gain-in" .kr(0.2)
      val gainMic   = 1.0 // 4.0 // "gain-out".kr(4.0)
      val outChan   = 0 // "out-ch"    .kr(0)
      val sigOut    = sweep * gainSpkr
      PhysicalOut.ar(outChan, sigOut)
      val sigIn     = PhysicalIn.ar * gainMic
      if (config.debug) sigIn.poll(0, "MIC")
      //DiskOut.ar("out", sigIn)
      val fftSize  = 2048
      val deConv   = PartConv.ar("sweep-rvs", sigIn, fftSize = fftSize)
      val buf       = Buffer.Empty(framesIRRec)
      val PRE_DELAY  = (3 * SR).toInt   // damn Pi chokes if we don't add a pre-delay
      val JACK_BLOCK_SIZE = 1024
      val JACK_NUM_BLOCKS = 3
      // there is a crazy variance in the latency between scsynth/jackd, up to several hundred sample frames.
      // no idea how to explain it. This one works in 80-90% of the cases
      val CONV_DELAY    = JACK_BLOCK_SIZE * JACK_NUM_BLOCKS + fftSize + config.unknownLatency
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

        // the first one is still badly cropped
        if (RunIdx() > 0) {

  //        val sweep = vrSweepEx.value
          val sweep     = vrSweep.value.toArray
  //        val normGain  = if (sweep.isEmpty) 1.0 else 1.0 / Math.max(1.0e-6, sweep.iterator.map(x => Math.abs(x)).max)
  //        val sweepNorm = sweep.map(_ * normGain)
  //        val numEmpty = sweep.segmentLength(_ == 0.0)
  //        println(s"numEmpty $numEmpty")
  //        println(sweepNorm./*drop(numEmpty).*/take(256).mkString(", "))

          normalize(sweep)
          prepareIR(sweep)
          val posBuf = new Array[Int](maxIRPos)
          val numPos = detectLocalMax(sweep, posBuf)

          val posBufT = posBuf.take(numPos)
          val cm      = posBufT.map(x => (x.toDouble / SR * (speedOfSound * 100)))
          val cmS     = cm.map(x => "%1.1f".format(x))

          println(posBufT .mkString("Pos (smp): ", ", ", ""))
          println(cmS     .mkString("Pos (cm ): ", ", ", ""))

          if (config.debug) {
            val afOut = AudioFile.openWrite(new File(dirAudio, s"_killme${RunIdx()}.aif"),
              AudioFileSpec(numChannels = 1, sampleRate = SR))
            afOut.write(Array(sweep))
            afOut.close()
          }
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

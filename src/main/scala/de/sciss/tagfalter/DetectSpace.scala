/*
 *  DetectSpace.scala
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
import de.sciss.log.Level
import de.sciss.lucre.Txn.{peer => peerTx}
import de.sciss.lucre.{Artifact, ArtifactLocation, DoubleObj, DoubleVector}
import de.sciss.numbers.Implicits._
import de.sciss.proc.{AudioCue, Proc, Runner, TimeRef, Universe}
import de.sciss.synth.SynthGraph
import de.sciss.synth.UGenSource.Vec
import de.sciss.tagfalter.Main.{SR, T, log}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import java.io.File
import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object DetectSpace {
  private final val durIRRec            = 3.0f // "total-dur-sec".kr(3)
  private final val framesIRRec         = (durIRRec * SR).toInt
  private final val threshExcessDefault = -30.dbAmp // -40.dbAmp
  private final val threshExcessMin     = -42.dbAmp // -40.dbAmp
  private final val threshExcessMax     = -18.dbAmp // -40.dbAmp
  private final val rangeLocalMax       = 128 // 128 / SR * speedOfSound is roughly a metre
  private final val bufLenIRPos         = framesIRRec / rangeLocalMax
  private final val speedOfSound        = 343.0f  // m/s
  private final val toleranceIRPosCM    = 10f // cm
  private final val cmPerFrame          = 1.0f / SR * (speedOfSound * 100)
  private final val NumIRRuns           = 4
  private final val minIRPosRepeat      = (NumIRRuns * 2.0f/3 + 0.5).toInt

  case class ConfigImpl(
                     debug          : Boolean = false,
//                     noise          : Boolean = false,
                     unknownLatency : Int     = -64, // frames
                     spaceCorrection: Float   = -26f, // cm
                     minSpacePos    : Int     =  6,
                     maxSpacePos    : Int     = 24,
                     dirAudio       : File    = new File("audio_work"),
                     sweepAmp       : Float   = -14f, // -12f, // -14f
                     numPosThresh   : Int     = 3,
                     jackBlockSize  : Int     = 1024,
                     jackNumBlocks  : Int     = 3,
                     spaceMaxCm     : Float   = 12000.0f,
                       ) extends Config

  trait Config {
    def debug           : Boolean
//    def noise           : Boolean
    def unknownLatency  : Int
    def spaceCorrection: Float
    def minSpacePos     : Int
    def maxSpacePos     : Int
    def dirAudio        : File
    /** Decibels */
    def sweepAmp        : Float
    def numPosThresh    : Int
    def jackBlockSize   : Int
    def jackNumBlocks   : Int
    def spaceMaxCm      : Float
  }

  case class Result(posCm: Vec[Float])

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      import org.rogach.scallop._

      printedName = "Tagfalter - DetectSpace"
      private val default = ConfigImpl()

      val unknownLatency: Opt[Int] = opt(default = Some(default.unknownLatency),
        descr = s"Add unknown latency in sample frames to IR measurement (default: ${default.unknownLatency}).",
      )
      val debug: Opt[Boolean] = toggle(default = Some(default.debug),
        descrYes = "Enter debug mode (verbosity, control files).",
      )
//      val noise: Opt[Boolean] = toggle(default = Some(default.noise),
//        descrYes = "Add background noise sound (testing).",
//      )
      val minSpacePos: Opt[Int] = opt(default = Some(default.minSpacePos),
        descr = s"Minimum number of spatial positions, center clipping will be adjusted (default: ${default.minSpacePos}).",
        validate = _ > 1
      )
      val maxSpacePos: Opt[Int] = opt(default = Some(default.maxSpacePos),
        descr = s"Maximum number of spatial positions, center clipping will be adjusted (default: ${default.maxSpacePos}).",
        validate = _ > 1
      )
      val spaceCorrection: Opt[Float] = opt(default = Some(default.spaceCorrection),
        descr = s"Add correction in cm to space measurements (default: ${default.spaceCorrection}).",
      )
      val dirAudio: Opt[File] = opt(default = Some(default.dirAudio),
        descr = s"Audio file directory (default: ${default.dirAudio})"
      )
      val sweepAmp: Opt[Float] = opt(default = Some(default.sweepAmp),
        descr = s"Sweep amplitude, in decibels (default: ${default.sweepAmp}).",
      )
      val numPosThresh: Opt[Int] = opt(default = Some(default.numPosThresh),
        descr = s"Number of threshold attempts when unifying positions (default: ${default.numPosThresh}).",
        validate = _ > 0
      )
      val jackBlockSize: Opt[Int] = opt(default = Some(default.jackBlockSize),
        descr = s"Jack Audio Server block size (default: ${default.jackBlockSize}).",
        validate = _ > 0
      )
      val jackNumBlocks: Opt[Int] = opt(default = Some(default.jackNumBlocks),
        descr = s"Jack Audio Server number of blocks (default: ${default.jackNumBlocks}).",
        validate = _ > 0
      )

      verify()
      implicit val config: Config = ConfigImpl(
        debug           = debug(),
        unknownLatency  = unknownLatency(),
//        noise           = noise(),
        spaceCorrection = spaceCorrection(),
        minSpacePos     = minSpacePos(),
        maxSpacePos     = maxSpacePos(),
        dirAudio        = dirAudio().getAbsoluteFile,
        sweepAmp        = sweepAmp(),
        numPosThresh    = numPosThresh(),
        jackBlockSize   = jackBlockSize(),
        jackNumBlocks   = jackNumBlocks(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    Main.boot { implicit tx => implicit universe => _ /*s*/ =>
      runBooted()
    }
  }

  def runBooted(/*s: Server*/)(implicit tx: T, universe: Universe[T], config: Config): Unit = {
//    val s = universe.auralContext.get.server
//
//    if (config.noise) {
//      // Note: this problem has been reduced to the use of DiskIn over Buffer/PlayBuf
//      // (SoundProcesses issue #117)
//      //
//      // (NOT:) the Pi HAT sound card "pauses" the alsa driver, or something like that,
//      // when it sees a zero signal. When it "resumes", we get random latencies.
//      // To avoid that, we add a permanent noise output.
//      println("Start background noise")
//      val gNoise = SynthGraph {
//        import de.sciss.synth.Import._
//        import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
//        val sig = WhiteNoise.ar(4.0e-4) // -68 dB -- minimum volume necessary to unblock audio driver
//        if (config.debug) sig.poll(0, "NOISE")
//        PhysicalOut.ar(0, sig)
//      }
//      /*val synNoise =*/ Synth.play(gNoise, nameHint = Some("noise"))(s)
//    }

    import universe.scheduler
    val now = scheduler.time
    scheduler.schedule(now + (TimeRef.SampleRate * 2).toLong) { implicit tx =>
      println("Start detect space")
      apply() { tx => _ =>
        tx.afterCommit {
          sys.exit()
        }
      }
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
    // Note: no longer applies the center clipping at this point, as
    // we might need to adjust the threshold on the fly

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
//      val y0ExcM = y0Abs - threshExcess
//      val y0Exc = if (y0ExcM >= 0.0) y0ExcM else 0.0
      val y0 = y0Abs // y0Exc
      out(i) = y0
      x2 = x1; x1 = x0
      i += 1
    }
  }

  // `out` should have a size of at least `maxIRPos`
  private def detectLocalMax(in: Array[Double], maxLen: Int, out: Array[Int], threshExcess: Double): Int = {
    val len = math.min(maxLen, in.length)

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
            // perform ad-hoc center clipping here
            val x1raw   = in(framesRead) - threshExcess
            val x1      = if (x1raw >= 0.0) x1raw else 0.0
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

  def apply()(done: T => Result => Unit)(implicit tx: T, universe: Universe[T], config: Config): Unit = {
    val p = Proc[T]()
    val dirAudio  = config.dirAudio // new File(userHome, "Documents/projects/Klangnetze/audio_work")
    val locAudio  = ArtifactLocation.newConst[T](dirAudio.toURI)
    val artFwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHz.aif"))
    val artBwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHzRvs.aif"))
    val specFwd   = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 2 * SR)
    val specBwd   = specFwd // AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 2 * SR)
    val cueFwd    = AudioCue.Obj[T](artFwd, specFwd, 0L, 1.0)
    val cueBwd    = AudioCue.Obj[T](artBwd, specBwd, 0L, 1.0)

    val RunIdx        = Ref(1) // (0)
    val ThreshExcess  = Ref(threshExcessDefault)

//    val posBufSq      = Array.ofDim[Int](NumIRRuns, bufLenIRPos)
    val posBufSqSq    = Array.ofDim[Int](NumIRRuns, config.numPosThresh, bufLenIRPos)
//    val numPosSq      = Array.ofDim[Int](NumIRRuns)
    val numPosSqSq    = Array.ofDim[Int](NumIRRuns, config.numPosThresh)
    val maxLen        = math.ceil(config.spaceMaxCm / cmPerFrame).toInt
    // println(s"maxLen $maxLen")

    p.graph() = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph.Ops.stringToControl
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
//      val sweep     = DiskIn.ar("in")
      val sweepBuf  = Buffer("in")
      val sweep     = PlayBuf.ar(1, sweepBuf, loop = 0)
      val gainSpkr  = "sweep-amp".kr(0.2f) // "gain-in" .kr(0.2)
      val gainMic   = 1.0f // 4.0 // "gain-out".kr(4.0)
      val outChan   = 0 // "out-ch"    .kr(0)
      val sigOut    = sweep * gainSpkr
      PhysicalOut.ar(outChan, sigOut)
      val sigIn     = PhysicalIn.ar * gainMic
      if (config.debug) sigIn.poll(0, "MIC")
      //DiskOut.ar("out", sigIn)
      val fftSize  = 2048
      val deConv   = PartConv.ar("sweep-rvs", sigIn, fftSize = fftSize)
      val buf       = Buffer.Empty(framesIRRec)
      val PRE_DELAY: Int  = 2 /*3 */ * SR // .toInt
      val convDelay     = config.jackBlockSize * config.jackNumBlocks + fftSize + config.unknownLatency
      val recRun        = ToggleFF.kr(TDelay.kr(Impulse.kr(0), (PRE_DELAY + convDelay).toFloat / SR))
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
    pAttr.put("sweep-amp", DoubleObj.newConst[T](config.sweepAmp.dbAmp))
    pAttr.put("out", vrSweep)
    val r = Runner(p)

    def nextRun()(implicit tx: T): Unit = {
      log.info(s"Running sweep rec (iteration ${RunIdx()})")
      r.run()
    }

    r.react { implicit tx => {
      case Runner.Done =>
        log.debug("(rec done)")

        // the first one is still badly cropped
        val runIdx = RunIdx()
        if (runIdx > 0) {

          val sweep = vrSweep.value.toArray

          normalize(sweep)
          prepareIR(sweep)
//          val posBuf = posBufSq(runIdx - 1) // new Array[Int](bufLenIRPos)
          val posBuf0 = posBufSqSq(runIdx - 1)(0) // new Array[Int](bufLenIRPos)
          val numPos0: Int = {

            @tailrec
            def tryDetect(attemptsLeft: Int): Int = {
              val t = ThreshExcess()
              val n = detectLocalMax(in = sweep, maxLen = maxLen, out = posBuf0, threshExcess = t)
              if (attemptsLeft == 0) n else {
                // move up and down in 3 dB steps
                if (n < config.minSpacePos && t > threshExcessMin) {
                  val t1 = t * 0.71f
                  log.debug(s"-- lowering threshold to $t1")
                  ThreshExcess() = t1
                  tryDetect(attemptsLeft = attemptsLeft - 1)
                } else if (n > config.maxSpacePos && t < threshExcessMax) {
                  val t1 = t * 1.413f
                  log.debug(s"-- raising threshold to $t1")
                  ThreshExcess() = t1
                  tryDetect(attemptsLeft = attemptsLeft - 1)
                } else {
                  n
                }
              }
            }

            tryDetect(attemptsLeft = if (runIdx == 1) 3 else 1)
          }
//          numPosSq(runIdx - 1) = numPos
          numPosSqSq(runIdx - 1)(0) = numPos0

          if (log.level <= Level.Debug) {
            val posBufT = posBuf0.take(numPos0)
            val cm      = posBufT.map(x => x.toFloat * cmPerFrame + config.spaceCorrection)
            val cmS     = cm.map(x => "%1.1f".format(x))

            log.debug(posBufT .mkString("Pos (smp): ", ", ", ""))
            log.debug(cmS     .mkString("Pos (cm ): ", ", ", ""))
          }

          var iLow = 1
          var tLow = ThreshExcess()
          while (iLow < config.numPosThresh) {
            tLow *= 0.71f
            val posBufLow = posBufSqSq(runIdx - 1)(iLow)
            val nLow = detectLocalMax(in = sweep, maxLen = maxLen, out = posBufLow, threshExcess = tLow)
            numPosSqSq(runIdx - 1)(iLow) = nLow
            iLow += 1
          }

          if (config.debug) {
            val afOut = AudioFile.openWrite(new File(dirAudio, s"_killme$runIdx.aif"),
              AudioFileSpec(numChannels = 1, sampleRate = SR))
            afOut.write(Array(sweep))
            afOut.close()
          }
        }

        if (RunIdx.transformAndGet(_ + 1) <= NumIRRuns) {
          nextRun()
        } else {

          // now unify the measurements
          def unify(i: Int): Vec[Vec[Float]] = {
            val cmAccAll: Array[Float] = posBufSqSq.iterator.zip(numPosSqSq).flatMap { case (posBufSq, numPosSq) =>
              val posBuf = posBufSq(i)
              val numPos = numPosSq(i)
              posBuf.iterator.take(numPos).map { x =>
                Math.max(0f, x * cmPerFrame + config.spaceCorrection)
              } .toArray[Float]
            } .toArray
            // val toleranceSmp  = toleranceIRPosCM / frameToCM
            val cmGroups = cmAccAll.sorted.foldLeft[Vec[Vec[Float]]](Vec.empty) {
              case (aggr, x) =>
                val last  = aggr.lastOption.getOrElse(Vec.empty)
                val merge = last.forall { y => (y absDif x) < toleranceIRPosCM }
                if (merge) {
                  val init = aggr.dropRight(1) // init
                  init :+ (last :+ x)
                } else {
                  aggr :+ Vec(x)
                }
            }
            val _posFound: Vec[Vec[Float]] = cmGroups.filter(_.size >= minIRPosRepeat)
            _posFound
          }

          var posFoundP = unify(0)
          var posFoundN = posFoundP
          var iLow      = 1
          while (posFoundN.size < config.minSpacePos && iLow < config.numPosThresh) {
            posFoundP = posFoundN
            posFoundN = unify(iLow)
            iLow += 1
          }
          val posFound = if (posFoundP.size >= config.minSpacePos && posFoundP.size <= config.maxSpacePos) {
            iLow -= 1
            posFoundP
          } else if (posFoundN.size >= config.minSpacePos && posFoundN.size <= config.maxSpacePos) {
            posFoundN
          } else if (config.minSpacePos.toFloat / math.max(1, posFoundP.size) < posFoundN.size.toFloat / config.maxSpacePos) {
            iLow -= 1
            posFoundP
          } else {
            posFoundN
          }

          val cmMean0: Vec[Float] = posFound.map(xs => xs.sum / xs.size) // .mean
          // make sure we have at least two values
          val cmMean: Vec[Float] = cmMean0 match {
            case Vec()        => Vec(123.4f, 492.1f)
            case Vec(single)  => Vec(single, single * 1.5f)
            case _            => cmMean0
          }

          log.info(s"Found ${cmMean0.size} stable positions in thresh $iLow (cm):")
          log.info(cmMean0.map(x => "%1.1f".format(x)).mkString(", "))

//          sys.exit()

          done(tx)(Result(cmMean))
        }

//      case Runner.Running =>
//        println("(rec running)")
//        new Thread {
//          override def run(): Unit = {
//            Thread.sleep(2000)
//            val s = de.sciss.synth.Server.default
//            s.dumpOSC(osc.Dump.Off)
//          }
//        } .start()

      case state =>
        log.debug(s"(rec $state)")
    }}

     nextRun()
  }
}

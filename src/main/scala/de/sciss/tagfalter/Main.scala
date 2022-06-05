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
import de.sciss.lucre.synth.{InMemory, Server}
import de.sciss.lucre.{Artifact, ArtifactLocation, DoubleVector, Workspace}
import de.sciss.osc
import de.sciss.proc.{AudioCue, AuralSystem, Proc, Runner, SoundProcesses, Universe}
import de.sciss.synth.proc.graph.PartConv
import de.sciss.synth.{Client, SynthGraph}

import java.io.File

object Main {
  type T = InMemory.Txn

  final val SR = 48000

  case class Config()

  def main(args: Array[String]): Unit = {
    implicit val config: Config = Config()
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
            s.peer.dumpOSC()
            SoundProcesses.step[T]("detect-space") { implicit tx =>
              implicit val ws: Workspace[T] = Workspace.Implicits.dummy[T]
              implicit val u: Universe[T] = Universe[T]()
              detectSpace()
            }
          }

        case _ => ()
      }}
      as.start(sCfg, cCfg)
    }
  }

  def detectSpace()(implicit tx: T, universe: Universe[T]): Unit = {
    val p = Proc[T]()
    val dirAudio  = new File(userHome, "Documents/projects/Klangnetze/audio_work")
    val locAudio  = ArtifactLocation.newConst[T](dirAudio.toURI)
    val artFwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHzPre.aif"))
    val artBwd    = Artifact[T](locAudio, Artifact.Child("sweep2s_48kHzRvs.aif"))
    val specFwd   = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 3 * SR)
    val specBwd   = AudioFileSpec(numChannels = 1, sampleRate = SR, numFrames = 2 * SR)
    val cueFwd    = AudioCue.Obj[T](artFwd, specFwd, 0L, 1.0)
    val cueBwd    = AudioCue.Obj[T](artBwd, specBwd, 0L, 1.0)

    p.graph() = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.proc.graph._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
       val sweep    = DiskIn.ar("in")
//      val sweepFreq = Line.ar(100.0, 16000.0, 2)
//      val sweep     = SinOsc.ar(sweepFreq)
//      sweepFreq.poll(4, "sweep-freq")
      val gainSpkr  = 0.2 // "gain-in" .kr(0.2)
      val gainMic   = 1.0 // 4.0 // "gain-out".kr(4.0)
      val outChan   = 0 // "out-ch"    .kr(0)
      val durTotal  = 3.0 // "total-dur-sec".kr(3)
      val sigOut    = sweep * gainSpkr
      PhysicalOut.ar(outChan, sigOut)
      val sigIn     = PhysicalIn.ar * gainMic
      sigIn.poll(0, "MIC")
      //DiskOut.ar("out", sigIn)
      val fftSize  = 2048
      val deConv   = PartConv.ar("sweep-rvs", sigIn, fftSize = fftSize)
      val framesTotal = durTotal * SR
      val buf     = Buffer.Empty(framesTotal)
      val PRE_DELAY  = (2.5 * SR).toInt   // damn Pi chokes if we don't add a pre-delay
      val JACK_BLOCK_SIZE = 1024
      val JACK_NUM_BLOCKS = 3
      // there is a crazy variance in the latency between scsynth/jackd, up to several thousand sample frames.
      // no idea how to explain it.
      val UNKNOWN_DELAY = -1024 //  -512 // 448 // -128 // 0 // 256 // 320
      val CONV_DELAY    = JACK_BLOCK_SIZE * JACK_NUM_BLOCKS + fftSize + UNKNOWN_DELAY
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
//    import universe.{cursor, workspace}
//    implicit val undo: UndoManager[T] = UndoManager.dummy[T]
//    implicit val ctx: Context[T] = Context[T](selfH = Some(tx.newHandle(p)))
//    val vrSweep = Var[Seq[Double]]()
//    val vrSweepEntry: Ex[(String, Seq[Double])] = ("out", vrSweep: Ex[Seq[Double]])
//    val vrSweepEx = vrSweep.expand[T]
//    val vrSweepEntryEx = vrSweepEntry.expand[T]
//    import ctx.targets
//    val map = new IExprAsRunnerMap[T](vrSweepEntryEx :: Nil, tx)
//    r.prepare(map)
    r.react { implicit tx => {
      case Runner.Done =>
        println("(rec done)")
//        val sweep = vrSweepEx.value
        val sweep     = vrSweep.value
        val normGain  = if (sweep.isEmpty) 1.0 else 1.0 / Math.max(1.0e-6, sweep.iterator.map(x => Math.abs(x)).max)
        val sweepNorm = sweep.map(_ * normGain)
//        val numEmpty = sweep.segmentLength(_ == 0.0)
//        println(s"numEmpty $numEmpty")
        println(sweepNorm./*drop(numEmpty).*/take(256).mkString(", "))

        {
          val afOut = AudioFile.openWrite(new File(dirAudio, "_killme.aif"),
            AudioFileSpec(numChannels = 1, sampleRate = SR))
          afOut.write(Array(sweepNorm.toArray))
          afOut.close()
        }
        sys.exit()

      case Runner.Running =>
        println("(rec running)")
        new Thread {
          override def run(): Unit = {
            Thread.sleep(2000)
            val s = de.sciss.synth.Server.default
            s.dumpOSC(osc.Dump.Off)
          }
        } .start()

      case state =>
        println(s"(rec state $state)")
    }}
    println("Running sweep rec")
    r.run()
  }
}

/*
 *  CalibrateGoertzel.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.synth.{RT, Server, Synth}
import de.sciss.synth.SynthGraph
import de.sciss.tagfalter.Main.T
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import scala.concurrent.stm.Ref

/*
  inside Reagenz, JBL C1 and DPA 4060:

f = 700.0
[dB]: 20.9966
f = 989.9494936611666
[dB]: 23.3599
f = 1400.0000000000002
[dB]: 18.256
f = 1979.8989873223336
[dB]: 26.4051
f = 2800.000000000001
[dB]: 27.6505
f = 3959.7979746446676
[dB]: 26.2137
f = 5600.000000000003
[dB]: 30.5289
f = 7919.595949289337
[dB]: 43.0954
f = 11200.000000000007
[dB]: 44.8702
f = 15839.191898578676
[dB]: 35.5524

  inside Reagenz, node 1, pretty ok:

f = 700.0
[dB]: 39.1585
f = 832.4449805019048
[dB]: 37.8518
f = 989.9494936611665
[dB]: 37.7705
f = 1177.2549813552002
[dB]: 41.8142
f = 1399.9999999999998
[dB]: 43.4558
f = 1664.8899610038093
[dB]: 40.0396
f = 1979.8989873223327
[dB]: 40.8883
f = 2354.5099627104
[dB]: 41.4988
f = 2799.999999999999
[dB]: 41.6057
f = 3329.7799220076176
[dB]: 41.3166
f = 3959.7979746446645
[dB]: 38.9389
f = 4709.019925420799
[dB]: 39.4412
f = 5599.999999999997
[dB]: 38.4891
f = 6659.559844015234
[dB]: 42.0176
f = 7919.595949289328
[dB]: 43.8803
f = 9418.039850841598
[dB]: 41.0457
f = 11199.999999999995
[dB]: 44.1965
f = 13319.119688030469
[dB]: 45.1957
f = 15839.191898578656
[dB]: 48.6599

  inside Reagenz (unten), node 2 (mic etwas vom lautsprecher entfernt):

f = 700.0
[dB]: -31.2255
f = 832.4449805019048
[dB]: -33.2912
f = 989.9494936611665
[dB]: -32.0194
f = 1177.2549813552002
[dB]: -25.357
f = 1399.9999999999998
[dB]: -29.3102
f = 1664.8899610038093
[dB]: -30.8028
f = 1979.8989873223327
[dB]: -29.7815
f = 2354.5099627104
[dB]: -36.7528
f = 2799.999999999999
[dB]: -31.7107
f = 3329.7799220076176
[dB]: -35.3729
f = 3959.7979746446645
[dB]: -39.2901
f = 4709.019925420799
[dB]: -34.7182
f = 5599.999999999997
[dB]: -35
f = 6659.559844015234
[dB]: -31.9544
f = 7919.595949289328
[dB]: -30.5223
f = 9418.039850841598
[dB]: -32.22
f = 11199.999999999995
[dB]: -36.2368
f = 13319.119688030469
[dB]: -40.2508
f = 15839.191898578656
[dB]: -40.5884


 */
object CalibrateGoertzel {

  case class ConfigImpl(
                         ampOut   : Double = 0.05,
                         ampIn    : Double = 24,
                         freqMin  : Double = 700.0,
                         freqMax  : Double = 16000.0,
                       ) extends Config

  trait Config {
    def ampOut  : Double
    def ampIn   : Double
    def freqMin : Double
    def freqMax : Double
  }

  def main(args: Array[String]): Unit = {
    Main.printInfo()

    object p extends ScallopConf(args) {

      import org.rogach.scallop._

      printedName = "Tagfalter - CalibrateGoertzel"
      private val default = ConfigImpl()

      val ampOut: Opt[Double] = opt(default = Some(default.ampOut),
        descr = s"Output signal amplitude, linear (default: ${default.ampOut}).",
      )
      val ampIn: Opt[Double] = opt(default = Some(default.ampIn),
        descr = s"Input microphone amplitude, linear (default: ${default.ampIn}).",
      )
      val freqMin: Opt[Double] = opt(default = Some(default.freqMin),
        descr = s"Minimum test frequency in Hertz (default: ${default.freqMin}).",
      )
      val freqMax: Opt[Double] = opt(default = Some(default.freqMax),
        descr = s"Maximum test frequency in Hertz (default: ${default.freqMax}).",
      )

      verify()
      implicit val config: Config = ConfigImpl(
        ampOut  = ampOut(),
        ampIn   = ampIn(),
        freqMin = freqMin(),
        freqMax = freqMax(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    Main.boot { implicit tx => implicit universe => s =>
      run(s)
    }
  }

  def run(s: Server)(implicit tx: T, config: Config): Unit = {
    val g = SynthGraph {
      import de.sciss.synth.Import._
      import de.sciss.synth.ugen.{DiskIn => _, PartConv => _, _}
      import de.sciss.synth.Ops.stringToControl

      val f1        = "freq".kr(700.0) // 1125.0 // 2250.0 // 4500.0 // 4240.0 // "freq-space".kr(1000) // Hz
      val bitPeriod = 160.0 // 80.0 // 160.0 // 80.0 // "bit-period".kr(40.0) // ms
      val bitFreq   = 1000.0 / bitPeriod
      val gHop      = 1.0 // 0.5 // 0.25 // 1.0
      val gMedian   = 5 // 9
      val gLen      = 1024 //  512 // 256 // 512 // 256 // 1024 // phaseLen / 8 // 4 // 256 // 512 // 1024 // * 2 // phaseLen // 1024
      val gLenH     = gLen/2

      val LAG_TIME = 0.01

      val pulse     = LFPulse.ar(bitFreq, Seq(0.0, 0.5))
      val pulseL    = Lag.ar(pulse, Seq(LAG_TIME, LAG_TIME))

      val sig = SinOsc.ar(f1) * config.ampOut * pulseL
      PhysicalOut.ar(0, sig)
      val in = PhysicalIn.ar * config.ampIn

      val g1        = Goertzel.kr(in, gLen, freq = f1, hop = gHop)
      val g1M0      = (g1.real.squared + g1.imag.squared).sqrt / gLenH
      val g1M       = Median.kr(g1M0, length = gMedian)
      val running   = RunningSum.kr(g1M, length = 512)

      val runSmp = TDelay.kr(Impulse.kr(1), 2.0)
      val integ = Integrator.kr(running * runSmp, 1.0)
      val cnt = PulseCount.kr(runSmp)
      //cnt.poll(runSmp, "COUNT")
      val N = 8
      val poll = cnt > (N - 0.5)

      (integ / N).ampDb.poll(poll, "[dB]")
      FreeSelf.kr(poll)
    }

    val freqRef = Ref(config.freqMin)

    def iterate()(implicit tx: RT): Unit = {
      val freqNow = freqRef()
      tx.afterCommit(println(s"f = $freqNow"))
      val syn = Synth.play(g)(s, Seq("freq" -> freqNow))
      syn.onEndTxn { implicit tx =>
//        val factor  = math.sqrt(2)
        val factor  = math.pow(2, 1.0/4)
        val freqNew = freqRef.transformAndGet(_ * factor)
        if (freqNew <= config.freqMax) iterate() else sys.exit()
      }
    }

    iterate()
  }
}

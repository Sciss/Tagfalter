/*
 *  OscNode.scala
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

import de.sciss.osc
import de.sciss.synth.Ops.ServerOps
import de.sciss.synth.Server
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

object OscNode {
  final val DEFAULT_PORT = 57120

  case class ConfigImpl(
                         dumpOsc: Boolean = false,
                         oscPort: Int     = DEFAULT_PORT,
                         useIp  : Boolean = false,
                       ) extends Config
  trait Config {
    def dumpOsc : Boolean
    def oscPort : Int
    def useIp   : Boolean
  }

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf(args) {
      printedName = "Tagfalter - OscNode"
      private val default = ConfigImpl()

      val dumpOsc: Opt[Boolean] = toggle(default = Some(default.dumpOsc),
        descrYes = "Dump incoming OSC messages to console",
      )
      val oscPort: Opt[Int] = opt(default = Some(default.oscPort),
        descr = s"OSC UDP port (default: ${default.oscPort}).",
      )
      val useIp: Opt[Boolean] = toggle(default = Some(default.useIp),
        descrYes = "Receive OSC on network IP address instead of loopback",
      )

      verify()
      implicit val config: Config = ConfigImpl(
        dumpOsc   = dumpOsc(),
        oscPort   = oscPort(),
        useIp     = useIp(),
      )
    }
    import p.config
    run()
  }

  def run()(implicit config: Config): Unit = {
    val oscCfg = osc.UDP.Config()
    oscCfg.localPort = config.oscPort
    if (!config.useIp) oscCfg.localIsLoopback = true
    val r = osc.UDP.Receiver(oscCfg)
    if (config.dumpOsc) r.dump()
    r.action = { (p, addr) =>
      p match {
        case osc.Message("/state", code) =>
          println(s"OSC: state is $code")
          // state 0 is the one we need to react to
          if (code == 0) sys.exit()

        case osc.Message("/tree") =>
          try {
            val s = Server.default
            println(s.counts)
            s.dumpTree(controls = true)

          } catch {
            case _: Exception =>
          }

        case osc.Message("/quit") =>
          sys.exit()

        case _ =>
          println(s"From $addr got $p")
      }
    }
    r.connect()
  }
}

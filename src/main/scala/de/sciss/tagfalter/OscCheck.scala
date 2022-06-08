package de.sciss.tagfalter

import de.sciss.osc

object OscCheck {
  def main(args: Array[String]): Unit = {
    val cfg = osc.UDP.Config()
    cfg.localPort       = 57120
    cfg.localIsLoopback = true
    val r = osc.UDP.Receiver(cfg)
    r.action = {
      case (msg, addr) =>
        println(s"From $addr got $msg") // Message(/state, 0) is the one we need to react to
    }
    r.connect()
  }
}

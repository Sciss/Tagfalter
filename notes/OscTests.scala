val addrL = {
  // val addr = java.net.InetAddress.getByName("klangpi01.local")
  // addr.getHostAddress
  java.net.InetAddress.getByName(s"${java.net.InetAddress.getLocalHost.getHostName}.local")
}

val tCfg = osc.UDP.Config()
tCfg.localAddress = addrL
val t = osc.UDP.Transmitter(tCfg)
t.connect()
val Seq(a1, a2, a3) = (1 to 3).map { i =>
   new java.net.InetSocketAddress(java.net.InetAddress.getByName(s"klangpi0$i.local").getHostAddress, 57120)
} 
t.send(osc.Message("/henlo to 1"), a1)
t.send(osc.Message("/henlo to 2"), a2)
t.send(osc.Message("/henlo to 3"), a3)

t.send(osc.Message("/quit"), a1)
t.send(osc.Message("/quit"), a2)
t.send(osc.Message("/quit"), a3)

t.send(osc.Message("/rec", 1.0f, 10f, "/home/pi/Documents/projects/Klangnetze_OLD/audio_work/_killme.aif"), a1)
t.send(osc.Message("/state", 1), a1)


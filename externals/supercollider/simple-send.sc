//// OSC

//// Send to an OSC listener running on port 9001

~host = NetAddr("localhost", 9001)

~host.sendMsg("/home", 42)


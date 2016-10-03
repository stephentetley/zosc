//// OSC

//// Listen on langPort (must it be langPort?)



~responder = OSCresponderNode(
	nil,
	'/home',
	{|time, rponder, addr |
		"received:".postln;
		[time, rponder, addr].postln;
	}
).add;


~host = NetAddr("localhost", NetAddr.langPort);
~host.sendMsg("/home", 1);



~responder.remove;
~host.disconnect;

NetAddr.langPort;
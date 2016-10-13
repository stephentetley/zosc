//// OSC

//// Listen on langPort (must it be langPort?)



~responder = OSCresponderNode(
	nil,
	'/home',
	{|time, rponder, msg |
		"received:".postln;
		[time, rponder, msg].postln;
	}
).add;


~host = NetAddr("localhost", NetAddr.langPort);
~host.sendMsg("/home", 1);

/// Messages are variadic and contain address as first element
~host.sendMsg("/home", 1, 3, "string");

/// Close down
~responder.remove;
~host.disconnect;

NetAddr.langPort;
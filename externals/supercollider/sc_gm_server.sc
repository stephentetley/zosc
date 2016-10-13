/// General MIDI sound server


MIDIClient.init;

m = MIDIOut(0);


(
t = TempoClock.new();
~play_note = { | chan, keynum, dur, velon=64, veloff=64 |
	m.noteOn(chan, keynum, velon);
	t.sched (dur, {m.noteOff(chan, keynum, veloff); }) ;
};
)


~responder1 = OSCresponderNode(
	nil,
	'/sc_gm_serve/play_note',
	{|time, rsp, msg |
		"received:".postln;
		[time, rsp, msg].postln;
		~play_note.value(0,60,2);
	}
).add;

~responder2 = OSCresponderNode(
	nil,
	'/sc_gm_serve/hands_down',
	{|time, rsp, msg |
		msg.postln;
		m.allNotesOff(msg[1]);
	}
).add;

/// test
~host = NetAddr("localhost", NetAddr.langPort);
~host.sendMsg("/sc_gm_serve/play_note");
~host.sendMsg("/sc_gm_serve/hands_down", 0);
~play_note.value(0,60,2);

/// Close down
~responder1.remove;
~responder2.remove;
~host.disconnect;

-module(basic_example).

-export([open/0, close/1, send/1, listen/0]).

-define(target_tty, "/dev/ttyUSB0").

open() ->
	SerialPort = serial:start([{open, ?target_tty}, {speed, 9600}]),
	{ok, SerialPort}.

close(SerialPort) ->
	SerialPort ! {close},
	ok.

send(SerialPort) ->
	SerialPort ! {send, "Hello World\r\n"},
	ok.

listen() ->
	receive

		% Receive data from the serial port on the caller's PID.
		{data, Bytes} ->
			io:format("~s", [Bytes]),
			listen()

	% Stop listening after 5 seconds of inactivity.
	after 5000 ->
		io:format("~n"),
		ok

  end.

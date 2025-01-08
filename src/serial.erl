%% Copyright (c) 1996, 1999 Johan Bevemyr
%% Copyright (c) 2007, 2009 Tony Garnock-Jones
%% Copyright (c) 2022, 2025 Olivier Boudeville
%%                    [olivier (dot) boudeville (at) esperide (dot) com]
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%    -*- Erlang -*-
%    File:	serial.erl  (~jb/serialport/serial.erl)
%    Author:	Johan Bevemyr
%    Created:	Tue Oct 22 14:07:24 1996
%    Purpose:

-module(serial).
-author('jb@erix.ericsson.se').

-export([start/0, start/1, start/2, init/1, init/2]).

-include("serial.hrl").


% Implementation notes:
%
% The simplest way of having the 'serial' (developed in C) executable return out
% of band information (i.e. log messages in addition to raw data) seems to add a
% one-byte selection header when serial sends a (binary) message to this Erlang
% part.
%
% If this selection header is:
%  - 0: then the rest of the binary is the actual data
%  - 1: then the rest of the binary is a log message

-define( data_selection_header,    0 ).
-define( message_selection_header, 1 ).


priv_dir() ->
	case code:priv_dir(serial) of

		{error, bad_name} ->
			"./priv";

		D ->
			D

	end.



start() ->
	start([]).



% From an escript, we might inherit from an incorrect 'priv' directory (e.g. the
% one of Oceanic), so we provide a way of setting it explicitly.
%
start(Options, SerialPrivDir) ->
	SerialPid = spawn_link(serial, init, [SerialPrivDir, self()]),
	[ SerialPid ! Opt || Opt <- Options ],
	SerialPid.

start(Options) ->
	start(Options, _SerialPrivDir=priv_dir()).



init(SerialPrivDir, ClientPid) ->
	process_flag(trap_exit, true),
	SerialPort = open_port(
		{spawn, SerialPrivDir ++ "/bin/serial -erlang"},
		_SpawnOpts=[binary, {packet, 2}, exit_status]
	),
	serial_loop(ClientPid, SerialPort).


init(Pid) ->
	init(_SerialPrivDir = priv_dir(), Pid).




serial_loop(ClientPid, SerialPort) ->

	receive

		% See implementation notes:
		{SerialPort, {data, <<?data_selection_header, Data/binary>>}} ->
			io:format("(data message of ~B bytes received from port ~w)~n",
					  [size(Data), SerialPort]),
			ClientPid ! {data, Data},
			serial_loop(ClientPid, SerialPort);

		{SerialPort, {data, <<?message_selection_header, Msg/binary>>}} ->
			io:format("(log message of ~B bytes received from port ~w)~n",
					  [size(Msg), SerialPort]),
			ClientPid ! {onSerialMessage, Msg},
			serial_loop(ClientPid, SerialPort);

		{SerialPort, {data, <<OtherSelHeader, Data/binary>>}} ->
			io:format("Error, the 'serial' port sent a message with "
				"unsupported selection header ~w (data ~p ignored).",
				[ OtherSelHeader, Data ] ),
			serial_loop(ClientPid, SerialPort);


		{send, Bytes} ->
			send_serial(SerialPort, [?SEND, Bytes]),
			serial_loop(ClientPid, SerialPort);

		connect ->
			send_serial(SerialPort, [?CONNECT]),
			serial_loop(ClientPid, SerialPort);

		disconnect ->
			send_serial(SerialPort, [?DISCONNECT]),
			serial_loop(ClientPid, SerialPort);

		{open, TTY} ->
			send_serial(SerialPort, [?OPEN, TTY]),
			serial_loop(ClientPid, SerialPort);

		close ->
			send_serial(SerialPort, [?CLOSE]),
			serial_loop(ClientPid, SerialPort);

		{speed, NewInSpeed, NewOutSpeed} ->
			send_serial(SerialPort, [
				?SPEED,
				integer_to_list(NewInSpeed),
				" ",
				integer_to_list(NewOutSpeed),
				0
			]),
			serial_loop(ClientPid, SerialPort);

		{speed, NewSpeed} ->
			send_serial(SerialPort, [
				?SPEED,
				integer_to_list(NewSpeed),
				" ",
				integer_to_list(NewSpeed),
				0
			]),
			serial_loop(ClientPid, SerialPort);

		parity_odd ->
			send_serial(SerialPort, [?PARITY_ODD]),
			serial_loop(ClientPid, SerialPort);

		parity_even ->
			send_serial(SerialPort, [?PARITY_EVEN]),
			serial_loop(ClientPid, SerialPort);

		break ->
			send_serial(SerialPort, [?BREAK]),
			serial_loop(ClientPid, SerialPort);

		report ->
			io:format("Serial report requested.~n", []),
			send_serial(SerialPort, [?REPORT]),

			% Replaced with a onSerialMessage message being sent back:

			%io:format("Serial report waited.~n", []),
			% Low probability that we receive an unrelated packet:
			%% receive

			%%	{SerialPort, {data, ReportBytes}} ->
			%%		%io:format("Serial report received: ~p.~n", [ReportBytes]),
			%%		ClientPid ! {receiveReport, [ReportBytes]}

			%% end,

			serial_loop(ClientPid, SerialPort);


		stop ->
			io:format("Stop requested, closing port ~w.~n", [SerialPort]),
			% Not knowing whether port shall be closed:
			send_serial(SerialPort, [?CLOSE]),
			stopped;

		% For a synchronous termination:
		{stop, RequesterPid} ->
			io:format(
				"Stop requested by ~w, closing port ~w.~n",
				[RequesterPid, SerialPort]
			),
			% Not knowing whether port shall be closed:
			send_serial(SerialPort, [?CLOSE]),
			RequesterPid ! serial_stopped,
			stopped;

		{'EXIT', SerialPort, Why} ->
			ClientPid ! {onSerialExitWithReason, [SerialPort, Why]},
			io:format("Serial port ~w exited with reason ~w.~n",
					  [SerialPort, Why]),
			exit(Why);

		{'EXIT', Linked, Why} ->
			ClientPid ! {onSerialExitLinked, [Linked, Why]},
			io:format("Linked ~w exited with reason ~w.~n", [Linked, Why]),
			exit(Why);

		{Port, {exit_status, Status} } ->
			ClientPid ! {onSerialExit, [Port, Status]},
			io:format("Port ~w exited, with status ~B.~n", [ Port, Status ] ),
			exit(Status);

		OtherError ->
			io:format("Received unknown message (ignored): '~w'.~n",
					  [OtherError]),
			serial_loop(ClientPid, SerialPort)

	end.


send_serial(SerialPort, Message) ->
	SerialPort ! {self(), {command, Message}}.

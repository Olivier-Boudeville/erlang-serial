%% Copyright (c) 1996, 1999 Johan Bevemyr
%% Copyright (c) 2007, 2009 Tony Garnock-Jones
%% Copyright (c) 2022 Olivier Boudeville
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
	Pid = spawn_link(serial, init, [SerialPrivDir, self()]),
	process_options(Pid, Options),
	Pid.

start(Options) ->
	start(Options, _SerialPrivDir = priv_dir()).

process_options(_Pid, []) ->
	done;
process_options(Pid, [Opt | Opts]) ->
	Pid ! Opt,
	process_options(Pid, Opts).

init(SerialPrivDir, Pid) ->
	process_flag(trap_exit, true),
	Port = open_port(
		{spawn, SerialPrivDir ++ "/bin/serial -erlang"},
		[binary, {packet, 2}]
	),
	serial_loop(Pid, Port).

init(Pid) ->
	init(_SerialPrivDir = priv_dir(), Pid).

serial_loop(Pid, Port) ->
	receive
		{Port, {data, Bytes}} ->
			Pid ! {data, Bytes},
			serial_loop(Pid, Port);
		{send, Bytes} ->
			send_serial(Port, [?SEND, Bytes]),
			serial_loop(Pid, Port);
		connect ->
			send_serial(Port, [?CONNECT]),
			serial_loop(Pid, Port);
		disconnect ->
			send_serial(Port, [?DISCONNECT]),
			serial_loop(Pid, Port);
		{open, TTY} ->
			send_serial(Port, [?OPEN, TTY]),
			serial_loop(Pid, Port);
		close ->
			send_serial(Port, [?CLOSE]),
			serial_loop(Pid, Port);
		{speed, NewInSpeed, NewOutSpeed} ->
			send_serial(Port, [
				?SPEED,
				integer_to_list(NewInSpeed),
				" ",
				integer_to_list(NewOutSpeed),
				0
			]),
			serial_loop(Pid, Port);
		{speed, NewSpeed} ->
			send_serial(Port, [
				?SPEED,
				integer_to_list(NewSpeed),
				" ",
				integer_to_list(NewSpeed),
				0
			]),
			serial_loop(Pid, Port);
		parity_odd ->
			send_serial(Port, [?PARITY_ODD]),
			serial_loop(Pid, Port);
		parity_even ->
			send_serial(Port, [?PARITY_EVEN]),
			serial_loop(Pid, Port);
		break ->
			send_serial(Port, [?BREAK]),
			serial_loop(Pid, Port);
		stop ->
			io:format("Stop requested, closing port ~w~n", [Port]),
			% Not knowing whether port shall be closed:
			send_serial(Port, [?CLOSE]),
			stopped;
		% For a synchronous termination:
		{stop, RequesterPid} ->
			io:format(
				"Stop requested by ~w, closing port ~w~n",
				[RequesterPid, Port]
			),
			% Not knowing whether port shall be closed:
			send_serial(Port, [?CLOSE]),
			RequesterPid ! serial_stopped,
			stopped;
		{'EXIT', Port, Why} ->
			io:format("Port exited with reason ~w~n", [Why]),
			exit(Why);
		{'EXIT', Linked, Why} ->
			io:format("Linked ~w exited with reason ~w~n", [Linked, Why]),
			exit(Why);
		OtherError ->
			io:format("Received unknown message ~w~n", [OtherError]),
			serial_loop(Pid, Port)
	end.

send_serial(Port, Message) ->
	Port ! {self(), {command, Message}}.

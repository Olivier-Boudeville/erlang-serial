![Erlang CI](https://github.com/Olivier-Boudeville/erlang-serial/workflows/Erlang%20CI/badge.svg?event=push)

# About erlang-serial

This is a port program with an Erlang driver for serial communication, originally written by Johan Bevemyr in 1996 and sporadically maintained by Tony Garnock-Jones from 2007 onwards.

This [specific repository](https://github.com/Olivier-Boudeville/erlang-serial) is a subsequent fork of these works, made by Olivier Boudeville (`olivier (dot) boudeville (at) esperide (dot) com`) in order to better support devices exposing a serial interface like Enocean gateways (refer to [Ceylan-Oceanic](https://oceanic.esperide.org/index.html#software-prerequisites) for further information) and presumably Arduino platforms.

The main changes done in this fork consists in:
- disabling the *Request to Send* (RTS) and *Clear to Send* (CTS) signals (RTS/CTS flow control), which prevented proper communication with said devices; this change was directly taken from [this commit](https://github.com/knewter/erlang-serial/commit/fb24371ed5d143836cc8eeab1e0680e03c1a0041)
- enabling a build also with rebar3 and adding a bit of continuous integration
- reformatting the legacy C and Erlang code for extra clarity

Many thanks to the original authors.


## Installation

This library is designed to run as an Erlang library, not an application dependency. To install this library, clone the library to a location of your choice and run the following from the command line:

```text
make && DESTDIR=/usr/lib make install
```

Adjust the `DESTDIR` path accordingly if Erlang is not installed at
`/usr/lib/erlang`. The `serial` module should now be accessible in Erlang, which
can be verified by running `erl`. `serial:start()` should return a PID.

```text
Erlang/OTP 17 [erts-6.4.1] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V6.4.1  (abort with ^G)
1> serial:start().
<0.35.0>
```

Our very specific way of installing erlang-serial, typically for [Ceylan-Oceanic](https://oceanic.esperide.org), is:

```text
$ mkdir -p ~/Software
$ cd ~/Software
$ git clone https://github.com/Olivier-Boudeville/erlang-serial.git
$ cd erlang-serial
$ make && DESTDIR=. make install
```

Afterwards one just has to ensure that the ``~/Software/erlang-serial/ebin`` directory is listed in one's code path.


Alternatively, one may use the rebar3 preliminary support that we added; run for that ``rebar3 compile``.


## Examples

The following examples are excerpts from `examples/basic_example.erl`.

Opening a connection to a USB serial adapter at 9600 baud:

```erlang
SerialPort = serial:start([{open, "/dev/ttyUSB0"}, {speed, 9600}])
```

Sending a message out the serial port:

```erlang
SerialPort ! {send, "Hello World\r\n"}
```

Data is received as a message to the process that called `serial:start()`. That process can handle the data by implementing a function like the following:

```erlang
listen() ->
  receive
	% Receive data from the serial port on the caller's PID:
	{data, Bytes} ->
	  io:format("~s", [Bytes]),
	  listen()
  after
	% Stop listening after 5 seconds of inactivity:
	5000 ->
	  io:format("~n"),
	  ok
  end.
```

See `examples/terminal.erl` for more example code (using the now-obsolete `gs` module).


## Debugging

One may rely on the free software ``cutecom`` in order to compare input/output as done from Erlang with manually-specified counterpart ones.

Note that, if modifying erlang-serial's source code, recompiling it is not enough, as it should be reinstalled as well. Run for example from its root: ``make && DESTDIR=. make install``.


## Additional Information

The C "legacy" code has been formatted as discussed in [this section](https://seaplus.esperide.org/#c-c-code-formatting) of Ceylan-Seaplus.

As for the formatting of the Erlang code, see [this section](https://howtos.esperide.org/Erlang.html#formatting-erlang-code) of Ceylan-HOWTO.


## License

Copyright (c) 1996, 1999 Johan Bevemyr

Copyright (c) 2007, 2009 Tony Garnock-Jones

Copyright (c) 2022       Olivier Boudeville

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

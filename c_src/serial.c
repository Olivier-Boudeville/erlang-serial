/*
Copyright (c) 1996, 1999 Johan Bevemyr
Copyright (c) 2007, 2009 Tony Garnock-Jones
Copyright (c) 2022, 2025 Olivier Boudeville
						 [olivier (dot) boudeville (at) esperide (dot) com]

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
*/

/*    -*- C -*-
 *    File: serial.c  (~jb/serialport/serial.c)
 *    Author: Johan Bevemyr
 *    Created: Fri Oct 18 09:59:34 1996
 *    Purpose: Provide Erlang with access to the serial port.
 */


/**
 * This program allows interacting with a serial port, seen as a TTY.
 *
 * It communicates through the following protocol when run with -erlang:
 *
 *   (all messages start with a two-byte header containing the size of the
 *   message payload)
 *
 *   SEND DATA
 *        Transmits DATA on the currently open tty
 *
 *   CONNECT
 *        Reopens a disconnected connection.
 *
 *   DISCONNECT
 *        Hangs up an open serial line
 *
 *   OPEN path
 *        Opens a new tty, closing the old (but not disconnecting). It
 *        is possible to alternate between two connections.
 *
 *   SPEED inspeed outspeed
 *        Sets the input and output speeds. New connections will
 *        have these speeds. It is also possible to change speed
 *        on the fly.
 *
 *   PARITY ODD/EVEN
 *        Sets the parity of the current connection.
 *
 *   BREAK
 *        Sends break.
 *
 *   REPORT
 *        Sends back a report; useful to check serial's health (no freeze).
 *
 * When this executable sends back information (through the output file
 * descriptor), the whole stream is prefixed with a (one-byte) selection header,
 * to specify the type of that information (e.g. data, log message).
 *
 *   usage: serial [-debug] [-cbreak] [-erlang] [-speed <bit rate>] [-tty <dev>]
 *          bit rate is one of
 *                  50      75      110
 *                  134     150     200
 *                  300     600     1200
 *                  1800    2400    4800
 *                  9600    19200   38400
 *                  57600   115200  230400
 */

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>

#include "serial.h"

int errno;

#define MAXSPEED 23
bit_rate bitrate_table[MAXSPEED] = {
	{0, B0},         {50, B50},         {75, B75},        {110, B110},
	{134, B134},     {150, B150},       {200, B200},      {300, B300},
	{600, B600},     {1200, B1200},     {1800, B1800},    {2400, B2400},
	{4800, B4800},   {9600, B9600},     {19200, B19200},  {38400, B38400},
	{57600, B57600}, {115200, B115200}, {230400, B230400}
};


#define DATA_SELECTION_HEADER    0
#define MESSAGE_SELECTION_HEADER 1


/**
 * Reports the specified error, with its code and message.
 *
 */
void report_error(unsigned int code, const char * message) {

	char full_msg[1024];

	snprintf(full_msg, sizeof(full_msg),
			 DEBUG_PREFIX "Error reported (%d): %s", code, message);

	perror(full_msg);

	exit(code);

}


/**
 * Returns the speed_t value associated with a given bit_rate according to the
 * bitrate_table. B0 is returned if no matching entry is found.
 *
 */
speed_t get_speed(int speed) {
	int i;

	for (i = 0; i < MAXSPEED; i++) {
		if (speed == bitrate_table[i].rate)
			break;
	}

	if (i == MAXSPEED)
		return B0;
	else
		return bitrate_table[i].speed;
}


/**
 * Configures the given tty for raw-mode.
 *
 */
void set_raw_tty_mode(int fd) {
	struct termios ttymodes;

	/* Get ttymodes */

	if (tcgetattr(fd, &ttymodes) < 0)
		report_error(1, "tcgetattr failed");


	/* Configure for raw mode (see man termios) */
	ttymodes.c_cc[VMIN] = 1;  /* at least one character */
	ttymodes.c_cc[VTIME] = 0; /* do not wait to fill buffer */

	ttymodes.c_iflag &=
		~(ICRNL |                            /* disable CR-to-NL mapping */
		  INLCR |                            /* disable NL-to-CR mapping */
		  IGNCR |                            /* disable ignore CR */
		  ISTRIP |                           /* disable stripping of eighth bit */
		  IXON |                             /* disable output flow control */
		  BRKINT |                           /* disable generate SIGINT on brk */
		  IGNPAR | PARMRK | IGNBRK | INPCK); /* disable input parity detection */

	ttymodes.c_lflag &= ~(ICANON | /* enable non-canonical mode */
						  ECHO |   /* disable character echo */
						  ECHOE |  /* disable visual erase */
						  ECHOK |  /* disable echo newline after kill */
						  ECHOKE | /* disable visual kill with bs-sp-bs */
						  ECHONL | /* disable echo nl when echo off */
						  ISIG |   /* disable tty-generated signals */
						  IEXTEN); /* disable extended input processing */

	ttymodes.c_cflag |= CS8;     /* enable eight bit chars */
	ttymodes.c_cflag &= ~PARENB; /* disable input parity check */
	ttymodes.c_cflag |= CREAD;   /* enable receiver */

	ttymodes.c_oflag &= ~OPOST; /* disable output processing */

	/* roland */
	ttymodes.c_cflag |= CLOCAL;

	/* Apply changes */

	if (tcsetattr(fd, TCSAFLUSH, &ttymodes) < 0)
		report_error(2, "tcsetattr failed");

}


/**
 * Sets input and output speeds of a given connection.
 */
void set_tty_speed(int fd, speed_t new_ispeed, speed_t new_ospeed) {
	struct termios ttymodes;

	/* Get ttymodes */

	if (tcgetattr(fd, &ttymodes) < 0)
		report_error(3, "tcgetattr failed");

	if (cfsetispeed(&ttymodes, new_ispeed) < 0)
		report_error(4, "cfsetispeed failed");

	if (cfsetospeed(&ttymodes, new_ospeed) < 0)
		report_error(5, "cfsetospeed failed");

	// ttymodes.c_cflag |= CRTSCTS;     /* enable RTS/CTS flow control */
	ttymodes.c_cflag &= ~CRTSCTS; /* disable RTS/CTS flow control */

	// Apply changes:
	if (tcsetattr(fd, TCSAFLUSH, &ttymodes) < 0)
		report_error(6, "tcsetattr failed");

}


/**
 * Returns the size of a two_byte_header message (from Erlang).
 */
int get_tbh_size(unsigned char buf[]) {
	return (((int)buf[0]) << 8) + ((int)buf[1]);
}


/**
 * Sets the first two bytes of the buffer to its size.
 */
void set_tbh_size(unsigned char buffer[], int size) {
	buffer[1] = (unsigned char)(size & 0xff);
	buffer[0] = (unsigned char)((size >> 8) & 0xff);
	return;
}


/**
 * Writes the specified applicative buffer (e.g. for data or log messages) to a
 * file descriptor, adding size information (2-byte header) at the beginning.
 *
 */
void tbh_write(int fd, const unsigned char buf[], int buffsize) {

	unsigned char header_buf[TBHSIZE];

	Debug1("tbh_write: send message of size %d.\r\n", buffsize);

	/* First, write two byte header */
	set_tbh_size(header_buf, buffsize);
	write(fd, header_buf, TBHSIZE);

	/* Second, write original buffer */
	write(fd, buf, buffsize);

	return;
}



/**
 * Writes the specified log message to the specified file descriptor, adding
 * first the relevant selection header.
 *
 */
void write_message(int outputfd, const char * message, unsigned char buf[]) {

	unsigned int msg_len = strlen(message);

	if (msg_len > MAXLENGTH)
		report_error(7, " write_message: too long");

	buf[0] = (unsigned char) MESSAGE_SELECTION_HEADER;
	strcpy((char *) buf+1, message);

	tbh_write(outputfd, buf, msg_len+1);

}


/**
 * Reads at least nr bytes from the specified file descriptor, and stores them
 * in buf.
 *
 * Returns the number of bytes read, or 0 if stream closed.
 */
int read_at_least(int fd, unsigned char buf[], int nr) {
	int remaining = nr;
	int nr_read = 0;

	while (remaining > 0) {
		int read_this_time;

		read_this_time = read(fd, &buf[nr_read], remaining);

		if (read_this_time == 0) /* Input stream closed? */
			return 0;

		nr_read += read_this_time;
		remaining -= read_this_time;
	}

	return nr_read;
}


/**
 * Reads one message with two-byte-header (hence from Erlang), filling buffer.
 *
 * Returns the number of elements used in the buffer, or 0 if the input file has
 * been closed.
 *
 */
int tbh_read(int fd, unsigned char buf[], int buffsize) {
	int remaining, msgsize;

	if (read_at_least(fd, buf, TBHSIZE) != TBHSIZE)
		return 0;

	remaining = get_tbh_size(buf);

	Debug1("tbh_read: got message of size %d.\r\n", remaining);

	msgsize =
		read_at_least(fd, &buf[TBHSIZE], Min(remaining, (buffsize - TBHSIZE)));

	if (msgsize == 0)
		return 0;
	else
		return msgsize + TBHSIZE;
}


/**
 * Writes a number of bytes found in the buffer to the tty (the serial port),
 * filling the buffer from the given fillfd if necessary.
 *
 */
void write_to_tty(int ttyfd, int fillfd, int totalsize, int buffsize,
				  unsigned char buf[], int buffmaxsize) {
	write(ttyfd, buf, buffsize);
	totalsize -= buffsize;

	while (totalsize > 0) {
		int readmax;

		readmax = Min(totalsize, buffmaxsize);
		buffsize = read(fillfd, buf, readmax);
		write(ttyfd, buf, buffsize);
		totalsize -= buffsize;
	}

	return;
}


/**********************************************************************/

int debug_enabled = FALSE;
//int debug_enabled = TRUE;


int main(int argc, char *argv[]) {

	int ttyfd = -1;            /* terminal file descriptor */

	int stdinfd;               /* user file descriptor     */
	int stdoutfd;              /* user out file descriptor */

	boolean cbreak = FALSE;    /* cbreak flag              */
	boolean erlang = FALSE;    /* talking to erlang flag   */
	speed_t in_speed = B9600;  /* default in speed         */
	speed_t out_speed = B9600; /* default out speed        */
	char ttyname[MAXPATHLEN];  /* terminal name            */

	// Debug messages are shown on the console, but are not visible from Erlang:
	//Debug("Testing Serial Debug\n");

	strcpy(ttyname, "/dev/ttyS0");

	// Processes the command-line arguments:

	{
		int i;

		for (i = 1; i < argc; i++) {
			if (strcmp(argv[i], "-cbreak") == 0) /* -cbreak */
			{
				cbreak = TRUE;
			} else if (strcmp(argv[i], "-debug") == 0) /* -debug */
			{
				debug_enabled = TRUE;
			} else if (strcmp(argv[i], "-speed") == 0) /* -speed  */
			{
				i += 1;
				if (i < argc) {
					out_speed = in_speed = get_speed(atoi(argv[i]));
					if (in_speed == B0)
						goto error_usage;
				} else
					goto error_usage;
			} else if (strcmp(argv[i], "-tty") == 0) /* -tty    */
			{
				i += 1;
				if (i < argc) {
					strncpy(ttyname, argv[i], MAXPATHLEN - 1);
				} else
					goto error_usage;
			} else if (strcmp(argv[i], "-erlang") == 0) /* -erlang */
			{
				erlang = TRUE;
			} else
				goto error_usage;
		}
	}

	// Configure serial port (tty):

	Debug1("TTY (serial port) name is %s.\r\n", ttyname);

	if (erlang) {
		Debug("In Erlang mode.\r\n");
	} else {
		Debug("In non-Erlang mode.\r\n");
		ttyfd = open(ttyname, O_RDWR);
		if (!TtyOpen(ttyfd)) {
			fprintf(stderr, "Serial: cannot open terminal %s for reading and writing.\n", ttyname);
			exit(8);
		}

		set_raw_tty_mode(ttyfd);
		set_tty_speed(ttyfd, in_speed, out_speed);
	}

	// Configure user (Erlang) port:

	stdinfd = fileno(stdin);
	stdoutfd = fileno(stdout);

	if (cbreak) {
		/* Use non-canonical mode for input */
		set_raw_tty_mode(stdinfd);
		fprintf(stderr, "Serial: entering non-canonical mode, exit with ---.\n");
	}

	// Start the serial processing loop:

	{
		fd_set readfds;   /* file descriptor bit field for select */
		int maxfd;        /* max file descriptor for select */

		// Incremented for the added one-byte selection header:
		unsigned char buf[MAXLENGTH+1]; /* buffer for transfer between serial-user */

		int escapes; /* number of consecutive escapes in cbreak */

		/* Set up initial bit field for select */
		maxfd = Max(stdinfd, ttyfd);
		FD_ZERO(&readfds);

		/* no escapes encountered yet */
		escapes = 0;

		while (TRUE) {

			int i;

			if (TtyOpen(stdinfd))
				FD_SET(stdinfd, &readfds);

			if (TtyOpen(ttyfd))
				FD_SET(ttyfd, &readfds);

			i = select(maxfd + 1, &readfds, NULLFDS, NULLFDS, NULLTV);

			if (i <= 0)
				report_error(9, "select failed");

			// Data read from TTY (serial port):
			if (TtyOpen(ttyfd) && FD_ISSET(ttyfd, &readfds)) /* from serial port */
			{
				int nr_read;

				Debug("Receiving from TTY\r\n");

				FD_CLR(ttyfd, &readfds);

				// Data message:

				unsigned char * payload_buffer ;

				if (erlang) {
					buf[0] = (unsigned char) DATA_SELECTION_HEADER;
					payload_buffer = buf+1;
				}
				else {
					payload_buffer = buf;
				}

				nr_read = read(ttyfd, payload_buffer, MAXLENGTH);

				if (nr_read <= 0) {
					fprintf(stderr, "serial: problem reading from tty.\n");
					exit(10);
				}

				Debug1("Received from TTY %d bytes.\r\n", nr_read);

				if (erlang) {
					// Not payload_buffer; one more (header) to write:
					tbh_write(stdoutfd, buf, nr_read+1);
				}
				else {
					write(stdoutfd, buf, nr_read);
				}
			}

			// Data read from the controlling Erlang process:
			if (TtyOpen(stdinfd) && FD_ISSET(stdinfd, &readfds)) /* from user */
			{
				int nr_read;
				int i;

				FD_CLR(stdinfd, &readfds);

				// Check for escape in cbreak mode:

				if (cbreak) {
					nr_read = read(stdinfd, buf, MAXLENGTH);

					for (i = 0; i < nr_read; i++) {
						if (buf[i] == '-') {
							escapes++;
							if (escapes == 3) {
								close(ttyfd);
								exit(11);
							}
						} else {
							escapes = 0;
						}
					}
					if (TtyOpen(ttyfd))
						write(ttyfd, buf, nr_read);
				}

				// Erlang mode:
				else if (erlang) {

					/* Messages from Erlang are structured as:
					 *   Length: 16
					 *   PacketType: 8
					 *   DATA
					 */

					nr_read = tbh_read(stdinfd, buf, MAXLENGTH);

					/* Check if stdin closed, i.e. controlling process
					 * terminated:
					 */
					if (nr_read == 0)
						report_error(12, "empty read");


					// Interpret packets from Erlang:
					switch (PacketType(buf)) {

					case SEND:
						Debug("Received SEND.\r\n");
						if (TtyOpen(ttyfd)) {
							write_to_tty(ttyfd, stdinfd, get_tbh_size(buf) - COMMANDSIZE,
										 nr_read - HEADERSIZE, &(buf[HEADERSIZE]),
										 MAXLENGTH - HEADERSIZE);
						}
						break;

					case CONNECT:
						Debug("Received CONNECT.\r\n");
						/* Reopen the current terminal */
						goto open;
						break;

					case DISCONNECT:
						Debug("Received DISCONNECT.\r\n");
						if (TtyOpen(ttyfd))
							set_tty_speed(ttyfd, B0, B0);
						goto close;
						break;

					case OPEN:
						Debug("Received OPEN.\r\n");
						/* Terminate string */
						buf[nr_read] = '\0';
						strcpy(ttyname, (char *) &buf[HEADERSIZE]);

open:
						Debug1("Opening %s.\r\n", ttyname);

						if (TtyOpen(ttyfd))
							close(ttyfd);

						ttyfd = open(ttyname, O_RDWR);
						maxfd = Max(stdinfd, ttyfd);

						if (!TtyOpen(ttyfd)) {
							fprintf(stderr,
								"Serial: cannot open terminal %s for reading ",
								&buf[HEADERSIZE]);
							fprintf(stderr, "and writing.\n");
							exit(13);
						}

						set_raw_tty_mode(ttyfd);
						set_tty_speed(ttyfd, in_speed, out_speed);
						break;

					case CLOSE:
						Debug("Received CLOSE.\r\n");
close:
						if (TtyOpen(ttyfd))
							close(ttyfd);
						ttyfd = -1;
						break;

					case SPEED:
					{
						int off;

						in_speed = get_speed(atoi(&buf[HEADERSIZE]));

						/* Null-terminated string */
						buf[nr_read] = '\0';

						/* Find start of second speed */
						for (off = HEADERSIZE; isdigit(buf[off]) && (off < MAXLENGTH);
								off += 1)
							;

						out_speed = get_speed(atoi(&buf[off]));

						Debug1("     raw SPEED %s\r\n", &buf[HEADERSIZE]);
						Debug2("Received SPEED %ud %ud.\r\n", (unsigned int) in_speed,
							   (unsigned int) out_speed);

						if (TtyOpen(ttyfd))
							set_tty_speed(ttyfd, in_speed, out_speed);
						break;
					}

					case PARITY_ODD:
						break;

					case PARITY_EVEN:
						break;

					case BREAK:
						if (TtyOpen(ttyfd))
							(void)tcsendbreak(ttyfd, BREAKPERIOD);
						break;

					case REPORT:
						Debug("Received REPORT\r\n");
						const char * report = "Serial is functional.";
						write_message(stdoutfd, report, buf);
						break;

					default:
						fprintf(stderr, "%s: unknown command from Erlang\n", argv[0]);
						break;
					}

				} else {
					nr_read = read(stdinfd, buf, MAXLENGTH);
					write(ttyfd, buf, nr_read);
				}

				if (nr_read <= 0) {
					fprintf(stderr, "Serial: problem reading from stdin.\n");
					exit(14);
				}
			}
		}
	}

	// Usage errors:

error_usage:
	fprintf(stderr,
			"usage: %s [-cbreak] [-erlang] [-speed <bit rate>] [-tty <dev>]\n",
			argv[0]);
	fprintf(stderr, "\tbit rate is one of \n\t\t50\t75\t110\n\t\t");
	fprintf(stderr, "134\t150\t200\n\t\t300\t");
	fprintf(stderr, "600\t1200\n\t\t1800\t2400\t4800\n\t\t");
	fprintf(stderr, "9600\t19200\t38400\n\t\t57600\t");
	fprintf(stderr, "115200\t230400\n");

	return 0;
}

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SERIAL_PROTOCOL_H_
#define SERIAL_PROTOCOL_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <cstdio>   /* Standard input/output definitions */
#include <linux/serial.h>
#include <sys/ioctl.h>
#include <iostream>
#include <string>
#include <sstream>

namespace msv {

    using namespace std;

    class SerialProtocolSample {
        public:

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misusey.
             * of the copy constructor.
             */
            SerialProtocolSample(const SerialProtocolSample &);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            SerialProtocolSample& operator=(const SerialProtocolSample &);

        public:
            /**
             * Constructor.
             */
            SerialProtocolSample();
            SerialProtocolSample(const string &, int);
            ~SerialProtocolSample();

			//Methods
			void connect(const string &);
			void disconnect();
			int writeToSerial(stringstream *ss);
			int fd;	//file descriptor for the port
			char *buf;
			int bufSize;
			bool portState;
    };
}

#endif /* SERIAL_PROTOCOL_H_ */



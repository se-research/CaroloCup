/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SERIAL_PROTOCOL_H_
#define SERIAL_PROTOCOL_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <cstdio>   /* Standard input/output definitions */
#include <linux/serial.h>
#include <sys/ioctl.h>
#include <iostream>
#include <sstream>

#include "core/base/Mutex.h"
#include "SerialDataListener.h"

namespace carolocup {

    using namespace std;

    class SerialProtocol {
        public:

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            SerialProtocol(const SerialProtocol &);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            SerialProtocol& operator=(const SerialProtocol &);

        public:
            /**
             * Constructor.
             */
            SerialProtocol();
            ~SerialProtocol();

			//Methods
			connect(string &);
			close();

            /**
             * This method sets the SerialDataListener.
             *
             * @param listener SerialDataListener to distribute the data.
             */
            void setSerialDataListener(SerialDataListener *listener);

        private:
            core::base::Mutex m_dataListenerMutex;
            ArduinoMegaDataListener *m_dataListener;
			int fd;	//file descriptor for the port
			bool portState = false;
			char *buf;

    };
}

#endif /* ARDUINO_MEGA_PROTOCOL_H_ */


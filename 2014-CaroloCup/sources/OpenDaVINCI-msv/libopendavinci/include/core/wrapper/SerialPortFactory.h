/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_SERIALPORTFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_SERIALPORTFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"
#include "core/wrapper/SerialPort.h"

namespace core {
    namespace wrapper {

        struct OPENDAVINCI_API SerialPortFactory
        {
            /**
             * This method creates a serial port.
             *
             * @param port Serial port.
             * @param baudRate Baud rate.
             * @return A new serial port
             */
            static SerialPort* createSerialPort(const string &port, const uint32_t &baudRate, const SerialPortSettings &settings = SerialPortSettings());
        };
    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_SERIALPORTFACTORY_H_*/

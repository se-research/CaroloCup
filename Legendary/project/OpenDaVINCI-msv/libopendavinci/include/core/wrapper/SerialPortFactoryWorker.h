/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_SERIALPORTFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_SERIALPORTFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"
#include "core/wrapper/SerialPortLibraryProducts.h"

#include "core/wrapper/SerialPort.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * SerialPortFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the SerialPortFactoryWorker template class.
         *
         * @See SerialPortFactory, SerialPortFactoryWorker,
         *      SerialPortLibraryProducts, BoostSerialPortFactoryWorker,
         *      POSIXSerialPortFactoryWorker
         *
         */

        template <SerialPortLibraryProducts product>
        class OPENDAVINCI_API SerialPortFactoryWorker
        {
            public:
                /**
                 * This method creates a serial port.
                 *
                 * @param port Port.
                 * @param baudRate Baud rate.
                 * @param settings SerialPortSettings (optional)
                 * @return A new serial port
                 */
                static SerialPort* createSerialPort(const string &port, const uint32_t &baudRate, const SerialPortSettings &settings = SerialPortSettings());
        };
    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_SERIALPORTFACTORYWORKER_H_*/

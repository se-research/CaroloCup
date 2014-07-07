/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSERIALPORTFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSERIALPORTFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SerialPortLibraryProducts.h"
#include "core/wrapper/SerialPortFactoryWorker.h"

#include "core/wrapper/POSIX/POSIXSerialPort.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API SerialPortFactoryWorker<SerialPortLibraryPosix>
        {
            public:
                static SerialPort* createSerialPort(const string &port, const uint32_t &baudRate, const SerialPortSettings &settings = SerialPortSettings())
                {
                    return new POSIX::POSIXSerialPort(port, baudRate, settings);
                };
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSERIALPORTFACTORYWORKER_H_*/

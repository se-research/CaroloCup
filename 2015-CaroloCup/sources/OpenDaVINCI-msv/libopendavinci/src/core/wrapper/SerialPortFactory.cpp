/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/SerialPortFactory.h"

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"
#include "core/wrapper/NetworkLibraryProducts.h"

#include "core/wrapper/SerialPortFactoryWorker.h"
#include "core/wrapper/SerialPort.h"

#ifdef HAVE_BOOST_LIBRARIES
    #include "core/wrapper/Boost/BoostSerialPortFactoryWorker.h"
#endif
#ifndef WIN32
    #include "core/wrapper/POSIX/POSIXSerialPortFactoryWorker.h"
#endif

namespace core {
    namespace wrapper {

        SerialPort* SerialPortFactory::createSerialPort(const string &port, const uint32_t &baudRate, const SerialPortSettings &settings)
        {
            typedef ConfigurationTraits<SerialPortLibraryProducts>::configuration configuration;

            return SerialPortFactoryWorker<configuration::value>::createSerialPort(port, baudRate, settings);
        }

    }
} // core::wrapper

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSERIALPORTFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSERIALPORTFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SerialPortFactoryWorker.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API SerialPortFactoryWorker<SerialPortLibraryBoost>
        {
            public:
                static SerialPort* createSerialPort(const string &port, const uint32_t &baudRate)
                {
                    #error "SerialPortFactory: NO IMPLEMENTATION FOR BOOST AVAILABLE YET!"
                    return NULL;
                };
        };
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSERIALPORTFACTORYWORKER_H_*/

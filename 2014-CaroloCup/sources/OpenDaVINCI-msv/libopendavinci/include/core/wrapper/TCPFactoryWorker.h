/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_TCPFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_TCPFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TCPAcceptor.h"
#include "core/wrapper/TCPConnection.h"
#include "core/wrapper/NetworkLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * TCPFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the TimeFactoryWorker template class.
         *
         * @See TCPFactory, TCPFactoryWorker,
         *      NetworkLibraryProducts, BoostTCPFactoryWorker,
         *      POSIXTCPFactoryWorker
         *
         */

        template <NetworkLibraryProducts product>
        struct OPENDAVINCI_API TCPFactoryWorker
        {
            static TCPAcceptor* createTCPAcceptor(const uint32_t &port);
            static TCPConnection* createTCPConnectionTo(const std::string& ip, const uint32_t& port);
        };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_TCPFACTORYWORKER_H_ */

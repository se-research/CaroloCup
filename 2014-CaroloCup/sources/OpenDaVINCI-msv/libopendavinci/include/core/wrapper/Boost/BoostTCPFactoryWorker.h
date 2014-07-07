/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TCPFactoryWorker.h"
#include "core/wrapper/Boost/BoostTCPAcceptor.h"
#include "core/wrapper/Boost/BoostTCPConnection.h"

namespace core {
    namespace wrapper {

        template <> struct OPENDAVINCI_API TCPFactoryWorker<NetworkLibraryBoostAsio>
        {
            static TCPAcceptor* createTCPAcceptor(const uint32_t &port)
            {
                return new Boost::BoostTCPAcceptor(port);
            };

            static TCPConnection* createTCPConnectionTo(const std::string& ip, const uint32_t& port)
            {
                return new Boost::BoostTCPConnection(ip, port);
            };
        };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPFACTORYWORKER_H_ */

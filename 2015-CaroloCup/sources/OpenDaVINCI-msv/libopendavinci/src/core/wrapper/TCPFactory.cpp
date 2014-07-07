/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/TCPFactory.h"

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"
#include "core/wrapper/TCPFactoryWorker.h"

#ifdef HAVE_BOOST_LIBRARIES
    #include "core/wrapper/Boost/BoostTCPFactoryWorker.h"
#endif

#ifndef WIN32
    #include "core/wrapper/POSIX/POSIXTCPFactoryWorker.h"
#endif

namespace core {
    namespace wrapper {

        TCPAcceptor* TCPFactory::createTCPAcceptor(const uint32_t &port)
        {
            typedef ConfigurationTraits<NetworkLibraryProducts>::configuration configuration;

            return TCPFactoryWorker<configuration::value>::createTCPAcceptor(port);
        }

        TCPConnection* TCPFactory::createTCPConnectionTo(const std::string& ip, const uint32_t& port)
        {
            typedef ConfigurationTraits<NetworkLibraryProducts>::configuration configuration;

            return TCPFactoryWorker<configuration::value>::createTCPConnectionTo(ip, port);
        }

    }
} // core::wrapper

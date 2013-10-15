/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/UDPFactory.h"

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"
#include "core/wrapper/NetworkLibraryProducts.h"

#include "core/wrapper/UDPFactoryWorker.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPReceiver.h"

#ifdef HAVE_BOOST_LIBRARIES
    #include "core/wrapper/Boost/BoostUDPFactoryWorker.h"
#endif
#ifndef WIN32
    #include "core/wrapper/POSIX/POSIXUDPFactoryWorker.h"
#endif

namespace core {
    namespace wrapper {

        UDPSender* UDPFactory::createUDPSender(const string &address, const uint32_t &port)
        {
            typedef ConfigurationTraits<NetworkLibraryProducts>::configuration configuration;

            return UDPFactoryWorker<configuration::value>::createUDPSender(address, port);
        }

        UDPReceiver* UDPFactory::createUDPReceiver(const string &address, const uint32_t &port)
        {
            typedef ConfigurationTraits<NetworkLibraryProducts>::configuration configuration;
            return UDPFactoryWorker<configuration::value>::createUDPReceiver(address, port);
        }

    }
} // core::wrapper

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXUDPFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXUDPFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/NetworkLibraryProducts.h"
#include "core/wrapper/UDPFactoryWorker.h"

#include "core/wrapper/POSIX/POSIXUDPReceiver.h"
#include "core/wrapper/POSIX/POSIXUDPSender.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API UDPFactoryWorker<NetworkLibraryPosix>
        {
            public:
                static UDPSender* createUDPSender(const string &address, const uint32_t &port)
                {
                    return new POSIX::POSIXUDPSender(address, port);
                };

                static UDPReceiver* createUDPReceiver(const string &address, const uint32_t &port)
                {
                    bool isMulticast = false;
                    string::size_type posFirstDot = address.find(".");
                    if (posFirstDot != string::npos) {
                        stringstream numericalValueStream(address.substr(0, posFirstDot));
                        uint32_t numericalValue = 0;
                        numericalValueStream >> numericalValue;
                        isMulticast = ( (numericalValue >= 224) && (numericalValue <= 239) );
                        clog << "Creating " << (isMulticast ? "multicast" : "regular") << " UDP receiver at " << address << ":" << port << "." << endl;
                    }
                    return new POSIX::POSIXUDPReceiver(address, port, isMulticast);
                };

        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXUDPFACTORYWORKER_H_*/

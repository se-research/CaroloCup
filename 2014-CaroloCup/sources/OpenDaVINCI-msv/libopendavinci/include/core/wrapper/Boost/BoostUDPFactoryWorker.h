/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/UDPFactoryWorker.h"

#include "core/wrapper/Boost/BoostUDPReceiver.h"
#include "core/wrapper/Boost/BoostUDPSender.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API UDPFactoryWorker<NetworkLibraryBoostAsio>
        {
            public:
                static UDPSender* createUDPSender(const string &address, const uint32_t &port)
                {
                    return new Boost::BoostUDPSender(address, port);
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

                    return new Boost::BoostUDPReceiver(address, port, isMulticast);
                };
        };
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPFACTORYWORKER_H_*/

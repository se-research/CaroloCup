/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>
#include <sstream>
#include <string>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/UDPFactory.h"
#ifndef WIN32
#include "core/wrapper/POSIX/POSIXUDPFactory.h"
#endif

namespace core {
    namespace wrapper {

        using namespace std;

        // Initialization of the singleton instance.
        Mutex* UDPFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        UDPFactory* UDPFactory::m_singleton = NULL;

        UDPFactory::UDPFactory() {}

        UDPFactory::~UDPFactory() {
            UDPFactory::m_singleton = NULL;
        }

        UDPReceiver* UDPFactory::createUDPReceiver(const string &address, const uint32_t &port) {
            // Check for multicast.
            bool isMulticast = false;
            string::size_type posFirstDot = address.find(".");
            if (posFirstDot != string::npos) {
                stringstream numericalValueStream(address.substr(0, posFirstDot));
                uint32_t numericalValue = 0;
                numericalValueStream >> numericalValue;
                isMulticast = ( (numericalValue >= 224) && (numericalValue <= 239) );
                clog << "Creating " << (isMulticast ? "multicast" : "regular") << " UDP receiver at " << address << ":" << port << "." << endl;
            }
            return getInstance().createUDPReceiver(address, port, isMulticast);
        }

        UDPFactory& UDPFactory::getInstance() {
            UDPFactory::m_singletonMutex->lock();
            {
                if (UDPFactory::m_singleton == NULL) {
                    switch (USESYSTEMLIBRARY) {
                    case POSIX_LIBRARIES:
#ifndef WIN32
                        UDPFactory::m_singleton = new POSIX::POSIXUDPFactory();
#endif
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval(UDPFactory::m_singleton);
                }
            }
            UDPFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper

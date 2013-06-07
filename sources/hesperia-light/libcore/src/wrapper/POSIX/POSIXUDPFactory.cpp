/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/POSIX/POSIXUDPFactory.h"
#include "core/wrapper/POSIX/POSIXUDPReceiver.h"
#include "core/wrapper/POSIX/POSIXUDPSender.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXUDPFactory::POSIXUDPFactory() {}

            POSIXUDPFactory::~POSIXUDPFactory() {}

            UDPSender* POSIXUDPFactory::createUDPSender(const string &address, const uint32_t &port) {
                return new POSIXUDPSender(address, port);
            }

            UDPReceiver* POSIXUDPFactory::createUDPReceiver(const string &address, const uint32_t &port, const bool &isMulticast) {
                return new POSIXUDPReceiver(address, port, isMulticast);
            }

        }
    }
} // core::wrapper::Boost

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/POSIX/POSIXTCPAcceptor.h"
#include "core/wrapper/POSIX/POSIXTCPFactory.h"
#include "core/wrapper/POSIX/POSIXTCPConnection.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXTCPFactory::POSIXTCPFactory() {}

            POSIXTCPFactory::~POSIXTCPFactory() {}

            TCPAcceptor* POSIXTCPFactory::createTCPAcceptor(const uint32_t& port) {
                TCPAcceptor* acceptor = new POSIXTCPAcceptor(port);
                return acceptor;
            }

            TCPConnection* POSIXTCPFactory::createTCPConnectionTo(const std::string& ip,
                    const uint32_t& port) {
                TCPConnection* connection = new POSIXTCPConnection(ip, port);
                return connection;
            }

        }
    }
} // core::wrapper::POSIX

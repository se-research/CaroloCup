/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTCPFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTCPFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TCPFactoryWorker.h"
#include "core/wrapper/POSIX/POSIXTCPAcceptor.h"
#include "core/wrapper/POSIX/POSIXTCPConnection.h"

namespace core {
    namespace wrapper {

        template <> struct OPENDAVINCI_API TCPFactoryWorker<NetworkLibraryPosix>
        {
            static TCPAcceptor* createTCPAcceptor(const uint32_t& port)
            {
                return new POSIX::POSIXTCPAcceptor(port);
            };

            static TCPConnection* createTCPConnectionTo(const string& ip, const uint32_t& port)
            {
                return new POSIX::POSIXTCPConnection(ip, port);
            };
        };
    }
}

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTCPFACTORY_H_*/

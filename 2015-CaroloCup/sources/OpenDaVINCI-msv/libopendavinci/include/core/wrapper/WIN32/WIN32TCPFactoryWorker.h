/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TCPFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TCPFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TCPFactoryWorker.h"
#include "core/wrapper/WIN32/WIN32TCPAcceptor.h"
#include "core/wrapper/WIN32/WIN32TCPConnection.h"

namespace core {
    namespace wrapper {

        template <> struct OPENDAVINCI_API TCPFactoryWorker<NetworkLibraryWin32>
        {
            static TCPAcceptor* createTCPAcceptor(const uint32_t& port)
            {
                return new WIN32Impl::WIN32TCPAcceptor(port);
            };

            static TCPConnection* createTCPConnectionTo(const string& ip, const uint32_t& port)
            {
                return new WIN32Impl::WIN32TCPConnection(ip, port);
            };
        };
    }
}

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TCPFACTORY_H_*/

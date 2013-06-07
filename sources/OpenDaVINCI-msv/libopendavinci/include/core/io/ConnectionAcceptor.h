/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_CONNECTIONACCEPTOR_H_
#define OPENDAVINCI_CORE_IO_CONNECTIONACCEPTOR_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Mutex.h"
#include "core/io/ConnectionAcceptorListener.h"
#include "core/wrapper/TCPAcceptor.h"
#include "core/wrapper/TCPAcceptorListener.h"

namespace core {
    namespace io {

        /**
         * This class is used to accept incoming connection. The
         * For every new Connection, the ConnectionAcceptorListener
         * is invoked with a new Connection object.
         *
         * To establish a connection, see #Connection
         */
        class OPENDAVINCI_API ConnectionAcceptor : public core::wrapper::TCPAcceptorListener {
            public:
                ConnectionAcceptor(const uint32_t port);
                virtual ~ConnectionAcceptor();

                void setConnectionAcceptorListener(ConnectionAcceptorListener* listener);

                void start();
                void stop();

            protected:
                ConnectionAcceptorListener* m_listener;
                core::wrapper::TCPAcceptor* m_acceptor;
                base::Mutex m_listenerMutex;

                void onNewConnection(core::wrapper::TCPConnection* connection);

            private:
                /**
                 * Forbidden copy constructor
                 */
                ConnectionAcceptor(const ConnectionAcceptor&);

                /**
                 * Forbidden assignement operator
                 */
                ConnectionAcceptor& operator=(const ConnectionAcceptor&);
        };
    }
}

#endif /* OPENDAVINCI_CORE_IO_CONNECTIONACCEPTOR_H_ */

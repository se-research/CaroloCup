/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_CONNECTIONACCEPTOR_H_
#define HESPERIA_CORE_IO_CONNECTIONACCEPTOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include <string>
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
        class HESPERIA_API ConnectionAcceptor : public core::wrapper::TCPAcceptorListener {
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

#endif /* HESPERIA_CORE_IO_CONNECTIONACCEPTOR_H_ */

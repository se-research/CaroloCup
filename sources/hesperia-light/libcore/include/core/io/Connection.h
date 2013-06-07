/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_CONNECTION_H_
#define HESPERIA_CORE_IO_CONNECTION_H_

#include "core/native.h"

#include <string>

#include "core/base/Mutex.h"
#include "core/data/Container.h"
#include "core/io/ConnectionErrorListener.h"
#include "core/io/ContainerObserver.h"
#include "core/wrapper/StringListener.h"
#include "core/wrapper/TCPConnection.h"


namespace core {
    namespace io {
        class HESPERIA_API Connection : public ContainerObserver,
                                        protected wrapper::StringListener,
                                        protected wrapper::ConnectionListener {

            public:
                Connection(const std::string& ip, const uint32_t port);
                virtual ~Connection();

                void send(core::data::Container& container);
                virtual void setContainerListener(ContainerListener *cl);
                void setErrorListener(ConnectionErrorListener* el);

                void start();
                void stop();

            protected:
                friend class ConnectionAcceptor;
                Connection(wrapper::TCPConnection* connection);

                virtual void nextString(const std::string &s);
                virtual void handleConnectionError();

            private:
                ContainerListener* m_listener;
                core::base::Mutex m_listenerMutex;
                wrapper::TCPConnection* m_connection;

                ConnectionErrorListener* m_errorHandler;
                core::base::Mutex m_errorHandlerMutex;

                /**
                 * Forbidden copy constructor
                 */
                Connection(Connection& obj);

                /**
                 * Forbidden assignement operator
                 */
                Connection& operator=(Connection& obj);
        };
    }
}

#endif /* HESPERIA_CORE_IO_CONNECTION_H_ */

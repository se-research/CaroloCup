/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/exceptions/Exceptions.h"
#include "core/io/Connection.h"
#include "core/wrapper/TCPFactory.h"

namespace core {
    namespace io {

        using namespace std;

        Connection::Connection(const std::string& ip, const uint32_t port)
                : m_listener(NULL),
                m_listenerMutex(),
                m_connection(NULL),
                m_errorHandler(NULL),
                m_errorHandlerMutex() {
            try {
                m_connection = wrapper::TCPFactory::createTCPConnectionTo(ip, port);
            } catch (string& s) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(ConnectException, s);
            }

            m_connection->setStringListener(this);
            m_connection->setConnectionListener(this);
        }

        Connection::Connection(wrapper::TCPConnection* connection)
                : m_listener(NULL),
                m_listenerMutex(),
                m_connection(connection),
                m_errorHandler(NULL),
                m_errorHandlerMutex() {
            m_connection->setStringListener(this);
            m_connection->setConnectionListener(this);
        }

        Connection::~Connection() {
            m_connection->setConnectionListener(NULL);
            m_connection->setStringListener(NULL);
            m_connection->stop();

            OPENDAVINCI_CORE_DELETE_POINTER(m_connection);
        }

        void Connection::send(core::data::Container& container) {
            container.setSentTimeStamp(core::data::TimeStamp());

            stringstream stringstreamValue;
            stringstreamValue << container;
            m_connection->send(stringstreamValue.str());
        }

        void Connection::nextString(const string &s) {
            core::base::Lock l(m_listenerMutex);

            if ( m_listener != NULL ) {
                stringstream stringstreamData(s);
                core::data::Container container;
                stringstreamData >> container;
                container.setReceivedTimeStamp(core::data::TimeStamp());

                // Inform the registered container listener about the new data.
                m_listener->nextContainer(container);
            }
        }

        void Connection::handleConnectionError() {
            core::base::Lock l(m_errorHandlerMutex);

            if (m_errorHandler != NULL ) {
                m_errorHandler->handleConnectionError();
            }
        }

        void Connection::setErrorListener(ConnectionErrorListener* el) {
            core::base::Lock l(m_errorHandlerMutex);
            m_errorHandler = el;
        }

        void Connection::setContainerListener(ContainerListener *cl) {
            core::base::Lock l(m_listenerMutex);
            m_listener = cl;
        }

        void Connection::start() {
            m_connection->start();
        }

        void Connection::stop() {
            m_connection->stop();
        }
    }
}

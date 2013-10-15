/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/io/ConnectionAcceptor.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/TCPFactory.h"

namespace core {
    namespace io {
        using namespace std;
        using namespace core::base;

        ConnectionAcceptor::ConnectionAcceptor(const uint32_t port)
                : m_listener(NULL),
                m_acceptor(NULL),
                m_listenerMutex() {
            try {
                m_acceptor = core::wrapper::TCPFactory::createTCPAcceptor(port);
            } catch (std::string s) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(ConnectionAcceptorException , s);
            }

            m_acceptor->setAcceptorListener(this);
        }

        ConnectionAcceptor::~ConnectionAcceptor() {
            m_acceptor->stop();
            m_acceptor->setAcceptorListener(NULL);
            OPENDAVINCI_CORE_DELETE_POINTER(m_acceptor);
        }

        void ConnectionAcceptor::setConnectionAcceptorListener(ConnectionAcceptorListener* listener) {
            Lock l(m_listenerMutex);
            m_listener = listener;
        }

        void ConnectionAcceptor::onNewConnection(core::wrapper::TCPConnection* connection) {
            Lock l(m_listenerMutex);
            if (m_listener != NULL) {
                m_listener->onNewConnection(new core::io::Connection(connection));
            } else {
                clog << "[ConnectionAcceptor|" << this << "] no listener to call" << endl;
            }
        }

        void ConnectionAcceptor::start() {
            m_acceptor->start();
        }

        void ConnectionAcceptor::stop() {
            m_acceptor->stop();
        }
    }
}

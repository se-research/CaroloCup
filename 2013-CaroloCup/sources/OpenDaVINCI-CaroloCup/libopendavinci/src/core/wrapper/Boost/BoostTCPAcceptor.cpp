/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <boost/bind.hpp>

#include "core/wrapper/Boost/BoostTCPAcceptor.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/ConcurrencyFactory.h"

namespace core {
    namespace wrapper {
        namespace Boost {
            BoostTCPAcceptor::BoostTCPAcceptor(uint32_t port)
                    : m_thread(NULL),
                    m_listenerMutex(NULL),
                    m_listener(NULL),
                    m_service(),
                    m_acceptor(m_service),
                    m_connection(NULL) {
                //std::clog << "[BoostTCPAcceptor|" << this <<"] c'tor("<< port << ")" << endl;

                m_thread = std::auto_ptr<Thread>(ConcurrencyFactory::createThread(*this));
                if (m_thread.get() == NULL) {
                    throw std::string("[BoostTCPAcceptor] Error creating thread");
                }

                m_listenerMutex = std::auto_ptr<Mutex>(MutexFactory::createMutex());
                if (m_listenerMutex.get() == NULL) {
                    throw std::string("[BoostTCPAcceptor] Error creating mutex");
                }

                //Setup asio
                //std::clog << "[BoostTCPAcceptor|" << this <<"] opening acceptor...";
                boost::system::error_code error;
                m_acceptor.open(boost::asio::ip::tcp::v4(), error);
                if (error) {
                    throw std::string("[BoostTCPAcceptor] Error opening socket: " + error.message());
                }
                //std::clog << "ok" << endl;

                //std::clog << "[BoostTCPAcceptor|" << this <<"] set reuse_address...";
                m_acceptor.set_option(boost::asio::socket_base::reuse_address(true), error);
                if (error) {
                    throw std::string("[BoostTCPAcceptor] Error setting reuse_address: " + error.message());
                }
                //std::clog << "ok" << endl;

                //std::clog << "[BoostTCPAcceptor|" << this <<"] bind...";
                boost::asio::ip::tcp::endpoint endpoint(boost::asio::ip::tcp::v4(), port);
                m_acceptor.bind(endpoint, error);
                if (error) {
                    throw std::string("[BoostTCPAcceptor] Error binding acceptor: " + error.message());
                }
                //std::clog << "ok" << endl;

                //std::clog << "[BoostTCPAcceptor|" << this <<"] placing acceptor in listening mode...";
                m_acceptor.listen(boost::asio::socket_base::max_connections, error);
                if (error) {
                    throw std::string("[BoostTCPAcceptor] Error placing acceptor in listening mode: " + error.message());
                }
                //std::clog << "ok" << endl;
            }

            BoostTCPAcceptor::~BoostTCPAcceptor() {
                setAcceptorListener(NULL);
                stop();

                if (m_connection) {
                    delete m_connection;
                    m_connection = NULL;
                }

            }

            void BoostTCPAcceptor::setAcceptorListener(TCPAcceptorListener* listener) {
                m_listenerMutex->lock();
                    m_listener = listener;
                m_listenerMutex->unlock();
            }

            void BoostTCPAcceptor::invokeAcceptorListener(TCPConnection* connection) {
                m_listenerMutex->lock();
                    if (m_listener != NULL) {
                        m_listener->onNewConnection(connection);
                    } else {
                        delete connection;
                    }
                m_listenerMutex->unlock();
            }

            void BoostTCPAcceptor::start() {
                m_thread->start();
                m_connection = new BoostTCPConnection();
                if (m_connection != NULL) {
                    m_acceptor.async_accept(m_connection->getSocket(),
                                            boost::bind(&BoostTCPAcceptor::acceptConnection,
                                                        this,
                                                        boost::asio::placeholders::error));
                } else {
                    std::clog << "[BoostTCPAcceptor|" << this << "] Error creating new TCPConnection for next accept" << std::endl;
                    m_connection = NULL;
                }
            }

            void BoostTCPAcceptor::stop() {
                m_service.stop();
                m_thread->stop();
            }

            bool BoostTCPAcceptor::isRunning() {
                return m_thread->isRunning();
            }

            void BoostTCPAcceptor::run() {
                boost::asio::io_service::work work(m_service);
                m_service.run();
                // The io_service was stopped and cannot be restarted.
                // Close the socket.
                boost::system::error_code error;
                m_acceptor.close(error);
                if (error) {
                    // This should never happen...
                    std::clog << "[BoostTCPAcceptor|" << this << "] Failed to close acceptor after io_service was stopped: " << error.message() << std::endl;
                }
            }

            void BoostTCPAcceptor::acceptConnection(const boost::system::error_code &error) {
                if (!error) {
                    invokeAcceptorListener(m_connection);
                    m_connection = new BoostTCPConnection();
                    if (m_connection != NULL) {
                        m_acceptor.async_accept(m_connection->getSocket(),
                                                boost::bind(&BoostTCPAcceptor::acceptConnection,
                                                            this,
                                                            boost::asio::placeholders::error));
                    } else {
                        std::clog << "[BoostTCPAcceptor|" << this << "] Error creating new TCPConnection for next accept" << std::endl;
                    }
                } else {
                    std::clog << "[BoostTCPAcceptor|" << this << "] Error accepting new connection: " << error.message() << std::endl;
                }
            }
        }
    }
}

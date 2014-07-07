/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <boost/bind.hpp>

#include "core/wrapper/Boost/BoostTCPConnection.h"
#include "core/wrapper/ConcurrencyFactory.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            BoostTCPConnection::BoostTCPConnection() :
                    m_thread(NULL),
                    m_service(),
                    m_socket(m_service)
            {
                //Create Thread
                m_thread = std::auto_ptr<Thread>(ConcurrencyFactory::createThread(*this));
                if (m_thread.get() == NULL) {
                    throw std::string("[BoostTCPConnection] Error creating thread");
                }
            }

            BoostTCPConnection::BoostTCPConnection(const std::string& ip, const uint32_t& port) :
                    m_thread(NULL),
                    m_service(),
                    m_socket(m_service)
            {
                //Create Thread
                m_thread = std::auto_ptr<Thread>(ConcurrencyFactory::createThread(*this));
                if (m_thread.get() == NULL) {
                    throw std::string("[BoostTCPConnection] Error creating thread");
                }

                //Setup Boost.Asio
                boost::system::error_code error;
                m_socket.open(boost::asio::ip::tcp::v4(), error);
                if (error) {
                    throw std::string("[BoostTCPConnection] Error opening socket: " + error.message() );
                }

                m_socket.set_option(boost::asio::socket_base::reuse_address(true), error);
                if (error) {
                    throw std::string("[BoostTCPConnection] Error setting reuse_address: " + error.message());
                }

                //TODO: Connecting without timeout could take some time...

                boost::asio::ip::tcp::endpoint endpoint(boost::asio::ip::address::from_string(ip), port);
                m_socket.connect(endpoint, error);
                if (error) {
                    std::stringstream ss;
                    ss << "[BoostTCPConnection] Error connecting to " << ip << ":" << port << ": " << error.message();
                    throw ss.str();
                }
                //std::clog << "[BoostTCPConnection] created" << std::endl;
            }

            BoostTCPConnection::~BoostTCPConnection() {
                stop();
            }

            void BoostTCPConnection::start() {
                m_thread->start();
                m_socket.async_receive(boost::asio::buffer(m_buffer, BUFFER_SIZE),
                                       boost::bind(&BoostTCPConnection::handleReceive, this,
                                                   boost::asio::placeholders::error,
                                                   boost::asio::placeholders::bytes_transferred));
            }

            void BoostTCPConnection::stop() {
                m_service.stop();
                m_thread->stop();
            }

            bool BoostTCPConnection::isRunning() {
                return m_thread->isRunning();
            }

            void BoostTCPConnection::run() {
                // The boost::asio::io_service::run() method blocks until pending
                // asynchronous operations are avaiable. The work instance causes
                // the run() method to block until the io_service is shutdown through
                // the io_service::stop() method.
                boost::asio::io_service::work work(m_service);
                m_service.run();

                // The io_service was stopped and cannot be restarted.
                // Close the socket.
                boost::system::error_code error;
                m_socket.close(error);
                if (error) {
                    // This should never happen...
                    std::clog << "[BoostTCPConnection] Failed to close socket after io_service was stopped: " << error.message() << std::endl;
                }
            }


            void BoostTCPConnection::handleReceive(const boost::system::error_code &error,
                                                   size_t nbytes) {
                if (!error) {
                    std::string data(m_buffer, nbytes);
                    // Delegate call to superclass.
                    receivedString(data);

                    m_socket.async_receive(boost::asio::buffer(m_buffer, BUFFER_SIZE),
                                           boost::bind(&BoostTCPConnection::handleReceive, this,
                                                       boost::asio::placeholders::error,
                                                       boost::asio::placeholders::bytes_transferred));
                } else {
                    invokeConnectionListener();
                    std::clog << "[BoostTCPConnection] Error receiving data: " << error.message() << std::endl;
                }
            }

            void BoostTCPConnection::sendImplementation(const std::string& data) {
                if (isRunning()) {
                    boost::system::error_code error;
                    m_socket.send(boost::asio::buffer(data), 0, error);

                    if (error) {
                        std::clog << "[BoostTCPConnection] Error sending data: " << error.message() << std::endl;
                        invokeConnectionListener();
                    }
                } else {
                    std::clog << "[BoostTCPConnection] ::send(...) called, but connection is not running" << std::endl;
                }
            }

            boost::asio::ip::tcp::socket& BoostTCPConnection::getSocket() {
                return m_socket;
            }
        }
    }
}

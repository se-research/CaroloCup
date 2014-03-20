/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <boost/bind.hpp>

#include "core/wrapper/ConcurrencyFactory.h"
#include "core/wrapper/StringListener.h"
#include "core/wrapper/Boost/BoostUDPReceiver.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            using namespace std;

            BoostUDPReceiver::BoostUDPReceiver(const string &address, const uint32_t port, const bool &isMulticast) :
                    m_ioService(),
                    m_senderEndpoint(),
                    m_socket(NULL),
                    m_thread(NULL) {
                // Create socket for receiving.
                m_socket = new boost::asio::ip::udp::socket(m_ioService);
                if (m_socket == NULL) {
                    stringstream s;
                    s << "Error while creating Boost socket at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                // Setup listener.
#ifdef WIN32
                string listenAddressString("0.0.0.0");
#else
                string listenAddressString(address);
#endif
                boost::asio::ip::udp::endpoint listenEndpoint(boost::asio::ip::address::from_string(listenAddressString), port);

                m_socket->open(listenEndpoint.protocol());
                m_socket->set_option(boost::asio::ip::udp::socket::reuse_address(true));
                m_socket->bind(listenEndpoint);

                // Check for multicast.
                if (isMulticast) {
                    // Set multicast address.
                    boost::asio::ip::address multicastAddress(boost::asio::ip::address::from_string(address));

                    // Join the multicast group.
                    m_socket->set_option(boost::asio::ip::multicast::join_group(multicastAddress));
                }

                // Register method for receiving.
                m_socket->async_receive_from(boost::asio::buffer(m_buffer, BUFFER_SIZE), m_senderEndpoint,
                                             boost::bind(&BoostUDPReceiver::handleReceiveFrom, this,
                                                         boost::asio::placeholders::error,
                                                         boost::asio::placeholders::bytes_transferred)
                                            );

                // Create thread for encapsulating ASIO.
                m_thread = auto_ptr<Thread>(ConcurrencyFactory::createThread(*this));
                if (m_thread.get() == NULL) {
                    stringstream s;
                    s << "Error while creating thread at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }
            }

            BoostUDPReceiver::~BoostUDPReceiver() {
                // Stop receiving.
                stop();

                if (m_socket != NULL) {
                    if (m_socket->is_open()) {
                        try {
#ifndef WIN32
                            m_socket->cancel();
#endif
                            m_socket->close();
                        } catch (std::exception &/*e*/) {
                            // Ignore exception while closing.
                        }
                    }

                    delete m_socket;
                }
                m_socket = NULL;

            }

            void BoostUDPReceiver::handleReceiveFrom(const boost::system::error_code &error, size_t nbytes) {
                if (!error) {
                    // ----------------------------------------v (remote address)-----v (data)
                    nextPacket(Packet(m_senderEndpoint.address().to_string(), string(m_buffer, nbytes)));

                    // Start next receiving.
                    m_socket->async_receive_from(boost::asio::buffer(m_buffer, BUFFER_SIZE), m_senderEndpoint,
                                                 boost::bind(&BoostUDPReceiver::handleReceiveFrom, this,
                                                             boost::asio::placeholders::error,
                                                             boost::asio::placeholders::bytes_transferred)
                                                );
                } else {
                    // TODO: Handle error!
                }
            }

            void BoostUDPReceiver::run() {
                // The following call will only return after a call to m_ioService.stop();
                m_ioService.run();
            }

            void BoostUDPReceiver::start() {
                m_thread->start();
            }

            void BoostUDPReceiver::stop() {
                // Interrupt ASIO IO service.
                m_ioService.stop();

                m_thread->stop();
            }

            bool BoostUDPReceiver::isRunning() {
                return m_thread->isRunning();
            }

        }
    }
} // core::wrapper::Boost

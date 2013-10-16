/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/Boost/BoostUDPSender.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            using namespace std;

            BoostUDPSender::BoostUDPSender(const string &address, const uint32_t port) :
                    m_ioService(),
                    m_endpoint(),
                    m_socket(NULL),
                    m_socketMutex(NULL) {
                // Create mutex.
                m_socketMutex = MutexFactory::createMutex();
                if (m_socketMutex == NULL) {
                    stringstream s;
                    s << "Error while creating mutex at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                // Create socket for sending.
                m_socket = new boost::asio::ip::udp::socket(m_ioService, m_endpoint.protocol());
                if (m_socket == NULL) {
                    stringstream s;
                    s << "Error while creating Boost socket at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                boost::asio::ip::address sendToAddress(boost::asio::ip::address::from_string(address));
                m_endpoint = boost::asio::ip::udp::endpoint(sendToAddress, port);
            }

            BoostUDPSender::~BoostUDPSender() {
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

                if (m_socketMutex != NULL) {
                    delete m_socketMutex;
                }
                m_socketMutex = NULL;
            }

            void BoostUDPSender::send(const string &data) const {
                if (data.length() > BoostUDPSender::MAX_UDP_PACKET_SIZE) {
                    stringstream s;
                    s << "Data to be sent is too large at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                m_socketMutex->lock();
                {
                    m_socket->send_to(boost::asio::buffer(data), m_endpoint);
                }
                m_socketMutex->unlock();
            }

        }
    }
} // core::wrapper::Boost

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/WIN32/WIN32UDPSender.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            using namespace std;

            WIN32UDPSender::WIN32UDPSender(const string &address, const uint32_t &port) :
                    m_address(),
                    m_fd(),
                    m_socketMutex(NULL) {
                m_socketMutex = MutexFactory::createMutex();
                if (m_socketMutex == NULL) {
                    stringstream s;
                    s << "Error while creating mutex at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

				// Load Winsock 2.2 DLL.
				WSADATA wsaData;
				if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
					stringstream s;
					const int retcode = WSAGetLastError();
					s << "Error while calling WSAStartUp at " << __FILE__ << ": " << __LINE__ << ": " << retcode;
					throw s.str();
				}

                // Create socket for sending.
                m_fd = ::socket(PF_INET, SOCK_DGRAM, 0);
                if (m_fd < 0) {
                    stringstream s;
					const int retcode = WSAGetLastError();
					s << "Error while creating file descriptor at " << __FILE__ << ": " << __LINE__ << ": " << retcode;

					// Decrement Winsock 2.2 DLL access counter.
					WSACleanup();

                    throw s.str();
                }

                // Setup address and port.
                memset(&m_address, 0, sizeof(m_address));
                m_address.sin_family = AF_INET;
                m_address.sin_addr.s_addr = inet_addr(address.c_str());
                m_address.sin_port = htons(port);
            }

            WIN32UDPSender::~WIN32UDPSender() {
                // Interrupt socket.
                ::shutdown(m_fd, SD_BOTH); // On POSIX: SHUT_RDWR

                // Close socket.
                ::closesocket(m_fd);

                if (m_socketMutex != NULL) {
                    delete m_socketMutex;
                }
                m_socketMutex = NULL;

				// Decrement Winsock 2.2 DLL access counter.
				WSACleanup();
            }

            void WIN32UDPSender::send(const string &data) const {
                if (data.length() > WIN32UDPSender::MAX_UDP_PACKET_SIZE) {
                    stringstream s;
                    s << "Data to be sent is too large at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                m_socketMutex->lock();
                {
					::sendto(m_fd, data.c_str(), data.length(), 0, (struct sockaddr *) &m_address, sizeof(m_address));
                }
                m_socketMutex->unlock();
            }

        }
    }
} // core::wrapper::WIN32Impl

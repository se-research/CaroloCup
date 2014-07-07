/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/POSIX/POSIXUDPSender.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            POSIXUDPSender::POSIXUDPSender(const string &address, const uint32_t port) :
                    m_address(),
                    m_fd(),
                    m_socketMutex(NULL) {
                m_socketMutex = MutexFactory::createMutex();
                if (m_socketMutex == NULL) {
                    stringstream s;
                    s << "Error while creating mutex at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                // Create socket for sending.
                m_fd = socket(PF_INET, SOCK_DGRAM, 0);
                if (m_fd < 0) {
                    stringstream s;
                    s << "Error while creating file descriptor at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                // Setup address and port.
                memset(&m_address, 0, sizeof(m_address));
                m_address.sin_family = AF_INET;
                m_address.sin_addr.s_addr = inet_addr(address.c_str());
                m_address.sin_port = htons(port);
            }

            POSIXUDPSender::~POSIXUDPSender() {
                // Interrupt socket.
                shutdown(m_fd, SHUT_RDWR);

                // Close socket.
                close(m_fd);

                if (m_socketMutex != NULL) {
                    delete m_socketMutex;
                }
                m_socketMutex = NULL;
            }

            void POSIXUDPSender::send(const string &data) const {
                if (data.length() > POSIXUDPSender::MAX_UDP_PACKET_SIZE) {
                    stringstream s;
                    s << "Data to be sent is too large at " << __FILE__ << ": " << __LINE__;
                    throw s.str();
                }

                m_socketMutex->lock();
                {
                    sendto(m_fd, data.c_str(), data.length(), 0, (struct sockaddr *) &m_address, sizeof(m_address));
                }
                m_socketMutex->unlock();
            }

        }
    }
} // core::wrapper::POSIX

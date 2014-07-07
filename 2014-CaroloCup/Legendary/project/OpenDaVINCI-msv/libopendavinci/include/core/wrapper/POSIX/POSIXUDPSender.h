/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXUDPSENDER_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXUDPSENDER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/UDPSender.h"

#include "core/wrapper/NetworkLibraryProducts.h"
#include "core/wrapper/UDPFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            /**
             * This class implements a UDP sender for sending data using POSIX.
             *
             * @See UDPSender
             */
            class POSIXUDPSender : public UDPSender {
                private:
                    enum {
                        MAX_UDP_PACKET_SIZE = 65507
                    };

                private:
                    friend class UDPFactoryWorker<NetworkLibraryPosix>;

                    /**
                     * Constructor.
                     *
                     * @param address Address of the receiver.
                     * @param port Port of the receiver.
                     */
                    POSIXUDPSender(const string &address, const uint32_t port);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXUDPSender(const POSIXUDPSender &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXUDPSender& operator=(const POSIXUDPSender &);

                public:
                    virtual ~POSIXUDPSender();

                    virtual void send(const string &data) const;

                private:
                    struct sockaddr_in m_address;
                    int32_t m_fd;

                    mutable Mutex *m_socketMutex;
            };

        }
    }
} // core::wrapper::POSIX

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXUDPSENDER_H_*/

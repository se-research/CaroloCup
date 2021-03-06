/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32UDPRECEIVER_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32UDPRECEIVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/UDPReceiver.h"

#include "core/wrapper/NetworkLibraryProducts.h"
#include "core/wrapper/UDPFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            using namespace std;

            /**
             * This class implements a UDP receiver for receiving data using WIN32.
             *
             * @See UDPReceiver
             */
            class WIN32UDPReceiver : public Runnable, public UDPReceiver {
                private:
                    friend class UDPFactoryWorker<NetworkLibraryWin32>;

                    /**
                        * Constructor.
                        *
                        * @param address Address to bind on.
                        * @param port Port.
                        * @param isMulticast true, if this receiver is part of the UDP multicast group specified by address:port.
                        */
                    WIN32UDPReceiver(const string &address, const uint32_t &port, const bool &isMulticast);

                private:
                    enum {
                        BUFFER_SIZE = 65535
                    };

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    WIN32UDPReceiver(const WIN32UDPReceiver &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    WIN32UDPReceiver& operator=(const WIN32UDPReceiver &);

                public:
                    virtual ~WIN32UDPReceiver();

                    virtual void start();

                    virtual void stop();

                private:
					const char* inet_ntop(int af, const void* src, char* dst, int cnt);

                private:
                    bool m_isMulticast;
                    struct sockaddr_in m_address;
                    struct ip_mreq m_mreq;
                    int32_t m_fd;
                    char *m_buffer;
                    Thread *m_thread;

                    virtual void run();

                    virtual bool isRunning();
            };

        }
    }
} // core::wrapper::WIN32Impl

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32UDPRECEIVER_H_*/

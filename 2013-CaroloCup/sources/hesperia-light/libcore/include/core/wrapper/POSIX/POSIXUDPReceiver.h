/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPRECEIVER_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPRECEIVER_H_

#include <arpa/inet.h>

#include <string>

#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/UDPReceiver.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            // Forward declaration to prevent circular dependencies.
            class POSIXUDPFactory;

            /**
             * This class implements a UDP receiver for receiving data using POSIX.
             *
             * @See UDPReceiver
             */
            class POSIXUDPReceiver : public Runnable, public UDPReceiver {
                private:
                    friend class POSIXUDPFactory;

                    /**
                        * Constructor.
                        *
                        * @param address Address to bind on.
                        * @param port Port.
                        * @param isMulticast true, if this receiver is part of the UDP multicast group specified by address:port.
                        */
                    POSIXUDPReceiver(const string &address, const uint32_t port, const bool &isMulticast);

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
                    POSIXUDPReceiver(const POSIXUDPReceiver &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXUDPReceiver& operator=(const POSIXUDPReceiver &);

                public:
                    virtual ~POSIXUDPReceiver();

                    virtual void start();

                    virtual void stop();

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
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPRECEIVER_H_*/

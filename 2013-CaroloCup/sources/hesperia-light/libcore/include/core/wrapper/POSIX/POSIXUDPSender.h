/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPSENDER_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPSENDER_H_

#include <arpa/inet.h>

#include <string>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/UDPSender.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            using namespace std;

            // Forward declaration to prevent circular dependencies.
            class POSIXUDPFactory;

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
                    friend class POSIXUDPFactory;

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

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPSENDER_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPSENDER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPSENDER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/asio.hpp>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/UDPSender.h"

#include "core/wrapper/NetworkLibraryProducts.h"
#include "core/wrapper/UDPFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            using namespace std;

            /**
             * This class implements a UDP sender for sending data using Boost.
             *
             * @See UDPSender
             */
            class BoostUDPSender : public UDPSender {
                private:
                    enum {
                        MAX_UDP_PACKET_SIZE = 65507
                    };

                private:
                    friend class UDPFactoryWorker<NetworkLibraryBoostAsio>;

                    /**
                     * Constructor.
                     *
                     * @param address Address of the receiver.
                     * @param port Port of receiver.
                     */
                    BoostUDPSender(const string &address, const uint32_t port);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BoostUDPSender(const BoostUDPSender &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BoostUDPSender& operator=(const BoostUDPSender &);

                public:
                    virtual ~BoostUDPSender();

                    virtual void send(const string &data) const;

                private:
                    boost::asio::io_service m_ioService;
                    boost::asio::ip::udp::endpoint m_endpoint;
                    boost::asio::ip::udp::socket *m_socket;

                    mutable Mutex *m_socketMutex;
            };

        }
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPSENDER_H_*/

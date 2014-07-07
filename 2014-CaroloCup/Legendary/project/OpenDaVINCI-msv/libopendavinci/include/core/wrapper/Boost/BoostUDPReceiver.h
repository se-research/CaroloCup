/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPRECEIVER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPRECEIVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/asio.hpp>

#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/UDPReceiver.h"

#include "core/wrapper/NetworkLibraryProducts.h"
#include "core/wrapper/UDPFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            using namespace std;

            /**
             * This class implements a UDP receiver for receiving data using Boost.
             *
             * @See UDPReceiver
             */
            class BoostUDPReceiver : public Runnable, public UDPReceiver {
                private:
                    enum {
                        BUFFER_SIZE = 65535
                    };

                private:
                    friend class UDPFactoryWorker<NetworkLibraryBoostAsio>;

                    /**
                     * Constructor.
                     *
                     * @param address Address to bind on.
                     * @param port Port.
                     * @param isMulticast true if this receiver should be part of the specified multicast group.
                     */
                    BoostUDPReceiver(const string &address, const uint32_t port, const bool &isMulticast);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BoostUDPReceiver(const BoostUDPReceiver &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BoostUDPReceiver& operator=(const BoostUDPReceiver &);

                public:
                    virtual ~BoostUDPReceiver();

                    virtual void start();

                    virtual void stop();

                private:
                    boost::asio::io_service m_ioService;
                    boost::asio::ip::udp::endpoint m_senderEndpoint;
                    boost::asio::ip::udp::socket *m_socket;
                    char m_buffer[BUFFER_SIZE];
                    auto_ptr<Thread> m_thread;

                    /**
                     * This method is called whenever some data is received.
                     *
                     * @param error Errorcode.
                     * @param nbytes Number of bytes received.
                     */
                    void handleReceiveFrom(const boost::system::error_code &error, size_t nbytes);

                    virtual void run();

                    virtual bool isRunning();
            };

        }
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTUDPRECEIVER_H_*/

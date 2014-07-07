/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPACCEPTOR_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPACCEPTOR_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/asio.hpp>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/TCPAcceptor.h"
#include "core/wrapper/TCPAcceptorListener.h"
#include "core/wrapper/Boost/BoostTCPConnection.h"

namespace core {
    namespace wrapper {
        namespace Boost {
            class OPENDAVINCI_API BoostTCPAcceptor : public TCPAcceptor,
                        public Runnable {
                public:
                    BoostTCPAcceptor(uint32_t port);
                    ~BoostTCPAcceptor();

                    virtual void setAcceptorListener(TCPAcceptorListener* listener);

                    virtual void start();
                    virtual void stop();

                    virtual bool isRunning();
                    virtual void run();

                protected:
                    std::auto_ptr<Thread> m_thread;

                    std::auto_ptr<Mutex> m_listenerMutex;
                    TCPAcceptorListener* m_listener;

                    boost::asio::io_service m_service;
                    boost::asio::ip::tcp::acceptor m_acceptor;

                    BoostTCPConnection* m_connection;

                    void acceptConnection(const boost::system::error_code &error);
                    void invokeAcceptorListener(TCPConnection* connection);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BoostTCPAcceptor(const BoostTCPAcceptor &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BoostTCPAcceptor& operator=(const BoostTCPAcceptor &);
            };
        }
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPACCEPTOR_H_ */

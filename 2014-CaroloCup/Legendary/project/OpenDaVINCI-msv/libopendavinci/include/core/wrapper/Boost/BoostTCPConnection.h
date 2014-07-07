/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPCONNECTION_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPCONNECTION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/asio.hpp>

#include "core/wrapper/Runnable.h"
#include "core/wrapper/TCPConnection.h"
#include "core/wrapper/Thread.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            /**
             * This class represents a Boost.Asio implementation of TCPConnection.
             * A BoostTCPConnections consists of a thread, a boost::asio::io_service instance
             * an a boost::asio::ip::tcp::socket instance. The socket describes the connection
             * between two TCP endpoints. The io_service is responsible for handling
             * asynchronous send and receive operations. The io_service is running in the
             * BoostTCPConnection's thread. Therefore, the registered StringListener instance
             * set through the TCPConnection::setStringListener() method is called from within
             * the BoostTCPConnection's thread.
             */
            class OPENDAVINCI_API BoostTCPConnection : public TCPConnection, public Runnable {
                public:
                    /**
                     * Creates a BoostTCPConnection without a connected socket. This
                     * constructor is used by the BoostTCPAcceptor. The BoostTCPAcceptor
                     * creates a BoostTCPConnection instance and uses the unconnected
                     * socket to accept incoming connections.
                     */
                    BoostTCPConnection();

                    /**
                     * Creates a BoostTCPConnection that is connected to the TCP endpoint
                     * specified by the given ip address and port. If the connection
                     * attempt fails, a exception of type std::string is thrown. If the
                     * constructor returns without throwing an exception, the connection
                     * is established but will not receive or send any data, until the
                     * start() method is called.
                     *
                     * @param ip
                     * @param port
                     * @throws std::string
                     */
                    BoostTCPConnection(const std::string& ip, const uint32_t& port);

                    virtual ~BoostTCPConnection();

                    virtual void sendImplementation(const std::string& data);

                    /**
                     * This method starts the thread of this BoostTCPConnection instance
                     * an a initial asynchronous read on the socket. The boost::asio::io_service
                     * is started within the run() method and blocks it, until the stop()
                     * method is called.
                     */
                    virtual void start();

                    /**
                     * The method stops the thread and the io_service. After calling
                     * the io_service::stop() method, the io_service will not further
                     * block the run() method and the thread will be destroyed.
                     */
                    virtual void stop();

                    /**
                     * Returns true if the connection is working, otherwise false.
                     */
                    virtual bool isRunning();


                    virtual void run();

                    /**
                     * Returns the underlying Boost.Asio socket. This method
                     * is only used by the BoostTCPAcceptor.
                     *
                     * @return The underlying Boost.Asio socket.
                     */
                    boost::asio::ip::tcp::socket& getSocket();

                protected:
                    /**
                     * The working thread for the boost::asio::io_service.
                     */
                    std::auto_ptr<Thread> m_thread;

                    /**
                     * The boost::asio::io_service is responsible for
                     * handling asynchronous send and receive operations.
                     */
                    boost::asio::io_service m_service;

                    /**
                     * The boost::asio::ip::tcp::socket describes the connection
                     * between two TCP endpoints.
                     */
                    boost::asio::ip::tcp::socket m_socket;

                    /**
                     * Defines the size of the read buffer.
                     */
                    enum { BUFFER_SIZE = 65535 };

                    /**
                     * The read buffer for incoming data.
                     */
                    char m_buffer[BUFFER_SIZE];

                    /**
                     * This method is used as a callback function for asynchronous receive
                     * operations. It is called by the boost::asio::io_service whenever
                     * (partial) data was received.
                     */
                    void handleReceive(const boost::system::error_code &error, size_t nbytes);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BoostTCPConnection(const BoostTCPConnection&);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BoostTCPConnection& operator=(const BoostTCPConnection&);
            };
        }
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTCPCONNECTION_H_ */

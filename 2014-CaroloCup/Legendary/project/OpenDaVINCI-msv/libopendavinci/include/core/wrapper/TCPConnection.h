/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_TCPCONNECTION_H_
#define OPENDAVINCI_CORE_WRAPPER_TCPCONNECTION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"
#include <sstream>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/StringObserver.h"
#include "core/wrapper/ConnectionObserver.h"

namespace core {
    namespace wrapper {

        using namespace std;

        class OPENDAVINCI_API TCPConnection : public StringObserver, public ConnectionObserver {
            protected:
                /**
                 * Protected constructor for enforcing subclasses for this class.
                 */
                TCPConnection();

            public:
                virtual ~TCPConnection();

                /**
                 * This method is used to send data using this TCP connection.
                 *
                 * @param data Data to send.
                 */
                void send(const string &data);

                /**
                 * This method registers a ConnectionListener that will be
                 * informed about connection errors.
                 *
                 * @param cl The ConnectionListener
                 */
                void setConnectionListener(ConnectionListener* cl);

                /**
                 * This method sets the StringListener that will receive
                 * incoming data.
                 *
                 * @param listener StringListener that will receive incoming data.
                 */
                void setStringListener(StringListener* listener);

                /**
                 * This method must be called to start the connection.
                 */
                virtual void start() = 0;

                /**
                 * This method closes a connection.
                 */
                virtual void stop() = 0;

            protected:
                /**
                 * This method has to be called by subclasses whenever
                 * new (partial) data was received. This method is
                 * responsible for gathering partial data and invoking
                 * the registered StringListener when a complete data
                 * packet was gathered.
                 *
                 * @param partialData Partially received data that will
                 *                    be gathered until the complete
                 *                    data can be passed to the registered
                 *                    StringListener.
                 */
                void receivedString(const string &partialData);

                /**
                 * This method has to be implemented in subclasses
                 * to send data. It is called from within the send()-
                 * method.
                 *
                 * param data Data with prepended size information.
                 */
                virtual void sendImplementation(const string& data) = 0;

                /**
                 * This method is called be subclasses to invoke
                 * the connection listener.
                 */
                void invokeConnectionListener();

            private:
                bool hasCompleteData();

                /**
                 * This method is used to pass received data thread-safe
                 * to the registered StringListener.
                 */
                void invokeStringListener(const string& data);

                std::auto_ptr<Mutex> m_connectionListenerMutex;
                ConnectionListener* m_connectionListener;

                std::auto_ptr<Mutex> m_stringListenerMutex;
                StringListener *m_stringListener;

                stringstream m_partialData;

                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                TCPConnection(const TCPConnection &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                TCPConnection& operator=(const TCPConnection &);
        };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_TCPCONNECTION_H_ */

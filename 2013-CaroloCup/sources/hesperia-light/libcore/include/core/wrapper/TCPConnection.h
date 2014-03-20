/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_TCPCONNECTION_H_
#define HESPERIA_CORE_WRAPPER_TCPCONNECTION_H_

#include <memory>
#include <string>
#include <sstream>

#include "core/native.h"
#include "core/wrapper/Mutex.h"
#include "core/wrapper/StringObserver.h"
#include "core/wrapper/ConnectionObserver.h"

namespace core {
    namespace wrapper {

        using namespace std;

        class HESPERIA_API TCPConnection : public StringObserver, public ConnectionObserver {
			private:
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
                 * This method is used to disable the automatic data defragmentation.
                 * Once the defragmentation is disable for a connection it cannot
                 * be enabled anymore.
                 */
                void disableAutomaticDataDefragmentation();

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
                 * This method is called by subclasses to invoke
                 * the connection listener.
                 */
                void invokeConnectionListener();

            private:
                std::auto_ptr<Mutex> m_connectionListenerMutex;
                ConnectionListener* m_connectionListener;

                std::auto_ptr<Mutex> m_stringListenerMutex;
                StringListener *m_stringListener;

                stringstream m_buffer;

                std::auto_ptr<Mutex> m_disableDefragmentationMutex;
                bool m_disableDefragmentation;

                /**
                 * This method is used to pass received data thread-safely
                 * to the registered StringListener.
                 */
                void invokeStringListener(const string& data);

                /**
                 * This method is used to decode a sent packet.
                 */
                const string decodePacket(const string &packet);
        };
    }
}

#endif /* HESPERIA_CORE_WRAPPER_TCPCONNECTION_H_ */

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS__TCPACCEPTORLISTENERMOCK_H
#define MOCKS__TCPACCEPTORLISTENERMOCK_H

#include "FunctionCallWaiter.h"

#include "core/wrapper/TCPAcceptorListener.h"
#include "core/wrapper/TCPConnection.h"

namespace mocks {
    class TCPAcceptorListenerMock : public core::wrapper::TCPAcceptorListener {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            TCPAcceptorListenerMock(const TCPAcceptorListenerMock& );

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            TCPAcceptorListenerMock& operator=(const TCPAcceptorListenerMock&);

            core::wrapper::TCPConnection* m_connection;

        public:
            TCPAcceptorListenerMock() :
                m_connection(NULL),
                CALLWAITER_onNewConnection()
            {};

            ~TCPAcceptorListenerMock() {
                if (m_connection) {
                    delete m_connection;
                }
            };

            core::wrapper::TCPConnection* getConnection() {
                return m_connection;
            }

            void dontDeleteConnection() {
                m_connection = NULL;
            }

             virtual void onNewConnection(core::wrapper::TCPConnection* connection) {
                m_connection = connection;
                CALLWAITER_onNewConnection.called();
            }

            FunctionCallWaiter CALLWAITER_onNewConnection;
    };
}
#endif

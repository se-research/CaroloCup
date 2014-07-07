/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS__CONNECTIONACCEPTORLISTENERMOCK_H
#define MOCKS__CONNECTIONACCEPTORLISTENERMOCK_H

#include <string>
#include <iostream>

#include "core/base/Condition.h"
#include "core/base/Lock.h"
#include "core/base/Mutex.h"

#include "core/io/ConnectionAcceptorListener.h"
#include "core/io/Connection.h"

namespace mocks {
    using namespace core;

    class ConnectionAcceptorListenerMock : public core::io::ConnectionAcceptorListener {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            ConnectionAcceptorListenerMock(const ConnectionAcceptorListenerMock& );

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            ConnectionAcceptorListenerMock& operator=(const ConnectionAcceptorListenerMock&);

        private:
            io::Connection* m_connection;
            bool m_connected;
            base::Condition m_condition;

        public:
            ConnectionAcceptorListenerMock() :
                    m_connection(NULL),
                    m_connected(false),
                    m_condition() {};

            ~ConnectionAcceptorListenerMock() {
                if (m_connection) {
                    delete m_connection;
                }
            };

            io::Connection* getConnection() {
                return m_connection;
            }

            void dontDeleteConnection() {
                m_connection = NULL;
            }

            void waitForConnection() {
                base::Lock l(m_condition);
                if (!hasConnection() ) {
                    m_condition.waitOnSignal();
                }
            }

            bool hasConnection() {
                return m_connected;
            }

            virtual void onNewConnection(io::Connection* connection) {
                base::Lock l(m_condition);
                m_connection = connection;
                m_connected = true;
                m_condition.wakeAll();
            }
    };
}
#endif

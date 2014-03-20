/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_TCPFACTORY_H_
#define HESPERIA_CORE_WRAPPER_TCPFACTORY_H_

#include "core/native.h"
#include "core/wrapper/Disposable.h"
#include "core/wrapper/TCPAcceptor.h"
#include "core/wrapper/TCPConnection.h"

namespace core {
    namespace wrapper {
        class HESPERIA_API TCPFactory : public Disposable {
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                TCPFactory(const TCPFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                TCPFactory& operator=(const TCPFactory &);

            protected:
                TCPFactory();
                TCPFactory* createBoostFactory();
                TCPFactory* createPOSIXFactory();

            public:
                virtual ~TCPFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static TCPFactory& getInstance();

                /**
                 * This method returns the wrapped TCP acceptor.
                 *
                 * @param port  Port for accepting TCP connections.
                 * @return a TCP acceptor
                 */
                virtual TCPAcceptor* createTCPAcceptor(const uint32_t &port) = 0;

                virtual TCPConnection* createTCPConnectionTo(const std::string& ip, const uint32_t& port) = 0;

            private:
                static Mutex *m_singletonMutex;
                static TCPFactory *m_singleton;
        };
    }
}

#endif /* HESPERIA_CORE_WRAPPER_TCPFACTORY_H_ */

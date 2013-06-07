/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_UDPFACTORY_H_
#define HESPERIA_CORE_WRAPPER_UDPFACTORY_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/Disposable.h"
#include "core/wrapper/Mutex.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPReceiver.h"

namespace core {
    namespace wrapper {

        /**
         * Abstract factory for creating wrapped UDP communication (i.e. based on Boost, POSIX, ...).
         *
         * @See UDPSender
         * @See UDPReceiver
         */
        class HESPERIA_API UDPFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                UDPFactory(const UDPFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                UDPFactory& operator=(const UDPFactory &);

            protected:
                UDPFactory();

            public:
                virtual ~UDPFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static UDPFactory& getInstance();

                /**
                 * This method returns the wrapped UDP sender.
                 *
                 * @param address Address.
                 * @param port Port.
                 * @return UDP sender based on the type of instance this factory is.
                 */
                virtual UDPSender* createUDPSender(const string &address, const uint32_t &port) = 0;

                /**
                 * This method returns the wrapped UDP receiver.
                 *
                 * @param address Address to bind on.
                 * @param port Port.
                 * @return UDP receiver based on the type of instance this factory is.
                 */
                UDPReceiver* createUDPReceiver(const string &address, const uint32_t &port);

            protected:
                /**
                 * This method returns the wrapped UDP receiver.
                 *
                 * @param address Address to bind on.
                 * @param port Port.
                 * @param isMulticast true, iff the sender should join a UDP multicast group specified by address/port.
                 * @return UDP receiver based on the type of instance this factory is.
                 */
                virtual UDPReceiver* createUDPReceiver(const string &address, const uint32_t &port, const bool &isMulticast) = 0;

            private:
                static Mutex *m_singletonMutex;
                static UDPFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_UDPFACTORY_H_*/

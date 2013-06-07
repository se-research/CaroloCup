/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPFACTORY_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPFACTORY_H_

#include "core/wrapper/UDPFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class is a concrete derivative for the abstract
             * factory UDPFactory.
             *
             * @See UDPFactory
             */
            class POSIXUDPFactory : public UDPFactory {
                protected:
                    friend class UDPFactory;

                    POSIXUDPFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXUDPFactory(const POSIXUDPFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXUDPFactory& operator=(const POSIXUDPFactory &);

                public:
                    virtual ~POSIXUDPFactory();

                    virtual UDPSender* createUDPSender(const string &address, const uint32_t &port);

                    virtual UDPReceiver* createUDPReceiver(const string &address, const uint32_t &port, const bool &isMulticast);
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXUDPFACTORY_H_*/

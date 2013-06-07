/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXTCPFACTORY_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXTCPFACTORY_H_

#include "core/wrapper/TCPFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            class POSIXTCPFactory : public TCPFactory {
                protected:
                    friend class TCPFactory;
                    POSIXTCPFactory();

                public:
                    virtual ~POSIXTCPFactory();

                    virtual TCPAcceptor* createTCPAcceptor(const uint32_t &port);
                    virtual TCPConnection* createTCPConnectionTo(const std::string& ip, const uint32_t& port);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXTCPFactory(const TCPFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXTCPFactory& operator=(const TCPFactory &);
            };
        }
    }
}

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXTCPFACTORY_H_*/

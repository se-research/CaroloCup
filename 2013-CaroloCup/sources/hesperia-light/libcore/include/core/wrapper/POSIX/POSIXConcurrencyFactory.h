/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXCONCURRENCYFACTORY_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXCONCURRENCYFACTORY_H_

#include "core/wrapper/ConcurrencyFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class is a concrete derivative for the abstract
             * factory ConcurrencyFactory.
             *
             * @See ConcurrencyFactory.
             */
            class POSIXConcurrencyFactory : public ConcurrencyFactory {
                protected:
                    friend class ConcurrencyFactory;

                    POSIXConcurrencyFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXConcurrencyFactory(const POSIXConcurrencyFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXConcurrencyFactory& operator=(const POSIXConcurrencyFactory &);

                public:
                    virtual ~POSIXConcurrencyFactory();

                    virtual Thread* createThread(Runnable &runnable);

                    virtual void usleep(const long &microseconds);
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXCONCURRENCYFACTORY_H_*/

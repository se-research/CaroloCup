/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXMUTEXFACTORY_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXMUTEXFACTORY_H_

#include "core/wrapper/MutexFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class is a concrete derivative for the abstract
             * factory MutexFactory.
             *
             * @See MutexFactory
             */
            class POSIXMutexFactory : public MutexFactory {
                protected:
                    friend class MutexFactory;

                    POSIXMutexFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXMutexFactory(const POSIXMutexFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXMutexFactory& operator=(const POSIXMutexFactory &);

                public:
                    virtual ~POSIXMutexFactory();

                    virtual Mutex* createMutex();
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXMUTEXFACTORY_H_*/

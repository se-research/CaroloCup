/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_CONCURRENCYFACTORY_H_
#define HESPERIA_CORE_WRAPPER_CONCURRENCYFACTORY_H_

#include "core/wrapper/Disposable.h"
#include "core/wrapper/Mutex.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"

namespace core {
    namespace wrapper {

        /**
         * Abstract factory for creating wrapped threads based
         * on Boost, pthread.
         *
         * @See Thread
         */
        class ConcurrencyFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ConcurrencyFactory(const ConcurrencyFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ConcurrencyFactory& operator=(const ConcurrencyFactory &);

            protected:
                ConcurrencyFactory();

            public:
                virtual ~ConcurrencyFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static ConcurrencyFactory& getInstance();

                /**
                 * This method returns the wrapped mutex.
                 *
                 * @param runnable The Runnable that should be threadified.
                 * @return Thread based on the type of instance this factory is.
                 */
                virtual Thread* createThread(Runnable &runnable) = 0;

                /**
                 * This methods sleeps for the specified amount of time.
                 *
                 * @param microseconds Time to sleep.
                 */
                virtual void usleep(const long &microseconds) = 0;

            private:
                static Mutex *m_singletonMutex;
                static ConcurrencyFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_CONCURRENCYFACTORY_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_MUTEXFACTORY_H_
#define HESPERIA_CORE_WRAPPER_MUTEXFACTORY_H_

#include "core/wrapper/Disposable.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {

        /**
         * Abstract factory for creating wrapped mutexes (i.e.
         * mutexes based on Boost, pthread, ...).
         *
            * It can be used as follows:
            *
            * @code
            * Mutex *m = MutexFactory::getInstance().createMutex();
            *
            * ...
            *
            * if (m != NULL) {
            *     m->lock();
            * }
            *
            * ...
            * // Do some things in a critical section.
            * ...
            *
            * if (m != NULL) {
            *     m->unlock();
            * }
            *
            * if (m != NULL) {
            *     delete m;
            * }
            *
            * @endcode
         */
        class MutexFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                MutexFactory(const MutexFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                MutexFactory& operator=(const MutexFactory &);

            protected:
                MutexFactory();

            public:
                virtual ~MutexFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static MutexFactory& getInstance();

                /**
                 * This method returns the wrapped mutex.
                 *
                 * @return mutex based on the type of instance this factory is.
                 */
                virtual Mutex* createMutex() = 0;

            private:
                static Mutex *m_singletonMutex;
                static MutexFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_MUTEXFACTORY_H_*/

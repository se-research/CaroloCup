/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_MUTEX_H_
#define HESPERIA_CORE_WRAPPER_MUTEX_H_

namespace core {
    namespace wrapper {

        /**
         * This interface encapsulates all methods necessary to
         * realize a mutex.
         *
         * @See MutexFactory
         */
        class Mutex {
            public:
                virtual ~Mutex();

                /**
                 * This method locks a wrapped mutex.
                 */
                virtual void lock() = 0;

                /**
                 * This method tries to lock a wrapped mutex.
                 *
                 * @return true, if the mutex could be locked.
                 */
                virtual bool tryLock() = 0;

                /**
                 * This method unlocks a wrapped mutex.
                 */
                virtual void unlock() = 0;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_MUTEX_H_*/

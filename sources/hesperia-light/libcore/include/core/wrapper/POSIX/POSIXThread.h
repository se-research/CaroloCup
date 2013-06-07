/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXTHREAD_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXTHREAD_H_

#include <pthread.h>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            // Forward declaration to prevent circular dependencies.
            class POSIXConcurrencyFactory;

            /**
             * This class is an implementation of the thread interface
             * using pthread.
             *
             * @See Thread
             */
            class POSIXThread : public Thread {
                private:
                    friend class POSIXConcurrencyFactory;

                    /**
                     * Constructor.
                     *
                     * @param r Runnable to be threadified.
                     */
                    POSIXThread(Runnable &r);

                private:
                    enum THREAD_STATE {
                        INITIALIZED,
                        RUNNING,
                        STOPPED
                    };

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXThread(const POSIXThread &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXThread& operator=(const POSIXThread &);

                public:
                    virtual ~POSIXThread();

                    virtual bool start();

                    virtual bool stop();

                    virtual bool isRunning() const;

                private:
                    Mutex *m_threadStateMutex;
                    THREAD_STATE m_threadState;

                    Runnable &m_runnable;

                    pthread_t m_threadWrapper;
            };

        }
    }
} // core::wrapper::POSIX

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXTHREAD_H_*/

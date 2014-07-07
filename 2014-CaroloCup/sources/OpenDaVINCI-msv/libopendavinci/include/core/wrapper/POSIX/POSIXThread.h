/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTHREAD_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTHREAD_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/SystemLibraryProducts.h"
#include "core/wrapper/ConcurrencyFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class is an implementation of the thread interface
             * using pthread.
             *
             * @See Thread
             */
            class POSIXThread : public Thread {
                private:
                friend class ConcurrencyFactoryWorker<SystemLibraryPosix>;

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

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTHREAD_H_*/

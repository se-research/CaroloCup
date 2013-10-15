/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTHREAD_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTHREAD_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/thread/thread.hpp>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/ConcurrencyFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            /**
             * This class is an implementation of the thread interface
             * using Boost.
             *
             * @See Thread
             */
            class BoostThread : public Thread {
                private:
                    friend class ConcurrencyFactoryWorker<SystemLibraryBoost>;

                    /**
                     * Constructor.
                     *
                     * @param r Runnable to be threadified.
                     */
                    BoostThread(Runnable &r);

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
                    BoostThread(const BoostThread &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BoostThread& operator=(const BoostThread &);

                public:
                    virtual ~BoostThread();

                    virtual bool start();

                    virtual bool stop();

                    virtual bool isRunning() const;

                private:
                    Mutex *m_threadStateMutex;
                    THREAD_STATE m_threadState;

                    Runnable &m_runnable;

                    ::boost::thread *m_boostThread;

                public:
                    /**
                     * This class encapsulates the thread handling for boost::thread.
                     */
                    class ThreadWrapper {
                        public:
                            /**
                             * Constructor.
                             *
                             * @param runnable Reference to the concurrent instance to be executed separately.
                             */
                            ThreadWrapper(Runnable &runnable);

                            /**
                             * Copy constructor.
                             *
                             * @param obj Reference to an object of this class.
                             */
                            ThreadWrapper(const ThreadWrapper &obj);

                        private:
                            /**
                             * "Forbidden" assignment operator. Goal: The compiler should warn
                             * already at compile time for unwanted bugs caused by any misuse
                             * of the assignment operator.
                             *
                             * @param obj Reference to an object of this class.
                             * @return Reference to this instance.
                             */
                            ThreadWrapper& operator=(const ThreadWrapper &/*obj*/) {
                                return (*this);
                            };

                        public:
                            void operator()();

                        private:
                            Runnable &m_runnable;

                    } m_threadWrapper;
            };

        }
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTHREAD_H_*/

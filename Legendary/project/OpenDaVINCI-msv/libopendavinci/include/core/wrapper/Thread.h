/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_THREAD_H_
#define OPENDAVINCI_CORE_WRAPPER_THREAD_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        /**
         * This class controls a thread. It can be used
         * to start different types of Runnables.
         *
         * It can be used as follows:
         *
         * @code
         * class MyRunnable : public Runnable {
         *      public:
         *          bool running;
         *          virtual bool isRunning() {
         *              // Implement some logic for controlling the running state.
         *              return running;
         *          }
         *
         *          virtual void run() {
         *              // Do some things right here.
         *          }
         * };
         * ...
         *
         * MyRunnable r;
         * r.running = false;
         * Thread *t = NULL;
         *
         * try {
         *     t = ConcurrencyFactory::getInstance().createThread(r);
         * }
         * catch(string &s) {
         *    clog << "Failed: " << s << endl;
         * }
         *
         * if (t != NULL) {
         *     r.running = true;
         *     t->start();
         * }
         *
         * ...
         * // Do some different things.
         * ...
         *
         * if (t != NULL) {
         *     r.running = false;
         *     t->stop();
         *     delete t;
         * }
         *
         * @endcode
         */
        class Thread {
            public:
                virtual ~Thread();

                /**
                 * This method starts a thread.
                 *
                 * @return true, iff the thread could be started.
                 */
                virtual bool start() = 0;

                /**
                 * This method stops a thread.
                 * A stopped thread cannot be restarted.
                 *
                 * @return true, iff the thread could be stopped.
                 */
                virtual bool stop() = 0;

                /**
                 * This method returns true, iff the thread is running.
                 *
                 * @return true, iff the thread is running.
                 */
                virtual bool isRunning() const = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_THREAD_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_REALTIMERUNNABLE_H_
#define OPENDAVINCI_CORE_WRAPPER_REALTIMERUNNABLE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Mutex.h"
#include "core/wrapper/Runnable.h"

namespace core {
    namespace wrapper {

        /**
         * This class provides a threadable interface for realtime,
         * periodic tasks. Only a POSIX implementation for Linux
         * with rt-preempt is available at the moment.
         *
         * It can be used as follows:
         *
         * @code
         * class MyRealtimeRunnable : public RealtimeRunnable {
         *      public:
         *          MyRealtimeRunnable() : RealtimeRunnable(100 * 1000)
         *          {}
         *
         *          virtual void nextTimeSlice() {
         *              // Do some things right here.
         *          }
         * };
         * ...
         *
         * MyRealtimeRunnable rtr;
         * Thread *t = NULL;
         *
         * try {
         *     t = ConcurrencyFactory::getInstance().createThread(rtr);
         * }
         * catch(string &s) {
         *    clog << "Failed: " << s << endl;
         * }
         *
         * if (t != NULL) {
         *     t->start();
         * }
         *
         * ...
         * // Do some different things.
         * ...
         *
         * if (t != NULL) {
         *     t->stop();
         *     delete t;
         * }
         *
         * @endcode
         */
        class OPENDAVINCI_API RealtimeRunnable : public Runnable {
            private:
                // Internal contants.
                enum {
                    MICROSECOND = 1000,                 // 1000 nanoseconds are one microsecond.
                    MILLISECOND = 1000 * MICROSECOND,   // 1000 microseconds are one millisecond.
                    SECOND      = 1000 * MILLISECOND,   // 1000 milliseconds are one second.

                    REALTIME_PRIORITY = 49 // PREMPT_RT use 50 for kernel tasklets and interrupt handler.
                };

                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                RealtimeRunnable(const RealtimeRunnable &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                RealtimeRunnable& operator=(const RealtimeRunnable &);

            protected:
                /**
                 * Constructor.
                 *
                 * @param periodInMicroseconds Duration for the slice.
                 */
                RealtimeRunnable(const long &periodInMicroseconds);

                /**
                 * This method is called periodically and must
                 * be implemented in derived classes.
                 */
                virtual void nextTimeSlice() = 0;

            public:
                virtual ~RealtimeRunnable();

            private:
                long m_periodInMicroseconds;

                virtual void run();
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_REALTIMERUNNABLE_H_*/

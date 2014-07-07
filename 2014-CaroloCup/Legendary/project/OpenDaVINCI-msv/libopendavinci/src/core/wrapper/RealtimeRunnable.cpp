/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/RealtimeRunnable.h"

namespace core {
    namespace wrapper {

        using namespace std;

        RealtimeRunnable::RealtimeRunnable(const long &periodInMicroseconds) :
                m_periodInMicroseconds(periodInMicroseconds) {
        }

        RealtimeRunnable::~RealtimeRunnable() {}

        void RealtimeRunnable::run() {
#ifdef HAVE_LINUX_RT
            // Setup realtime task using FIFO scheduling.
            struct sched_param param;
            param.sched_priority = REALTIME_PRIORITY;

            if (::sched_setscheduler(0, SCHED_FIFO, &param) == -1) {
                throw string("Failed to configure scheduler. Are you superuser?");
            }

            long delta = 0;
            struct timespec beforeSlice;
            struct timespec afterSlice;
            struct timespec waitForSlice;

            // Get actual time.
            ::clock_gettime(CLOCK_REALTIME , &waitForSlice);

            // Take next time slice.
            waitForSlice.tv_sec++;

            // Main realtime execution loop.
            while (isRunning()) {
                // Wait for next time slice.
                ::clock_nanosleep(CLOCK_REALTIME, TIMER_ABSTIME, &waitForSlice, NULL);

                // Get time before slice.
                ::clock_gettime(CLOCK_REALTIME, &beforeSlice);

                // Execute slice.
                nextTimeSlice();

                // Get time after slice.
                ::clock_gettime(CLOCK_REALTIME, &afterSlice);

                // Compute delta.
                delta = afterSlice.tv_nsec - beforeSlice.tv_nsec;

                if (isRunning() && !((m_periodInMicroseconds - (delta / MICROSECOND)) > 0)) {
                    throw string("Time slice ran out of time defined in microseconds!");
                }

                // Calculate waiting period.
                const uint32_t duration = (m_periodInMicroseconds - (delta / MICROSECOND));
                const uint32_t seconds = (duration / (SECOND / MICROSECOND));
                const uint32_t microseconds = (duration % (SECOND / MICROSECOND));
                waitForSlice.tv_sec += seconds;
                waitForSlice.tv_nsec += (microseconds * MICROSECOND);
                while (waitForSlice.tv_nsec >= SECOND) {
                    waitForSlice.tv_nsec -= SECOND;
                    waitForSlice.tv_sec++;
                }
            }
#else
            throw string("Realtime is only available on Linux with rt-preempt.");
#endif
        }

    }
} // core::wrapper

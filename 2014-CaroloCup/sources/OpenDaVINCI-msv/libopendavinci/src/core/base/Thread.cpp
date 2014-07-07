/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Thread.h"
#include "core/wrapper/ConcurrencyFactory.h"

namespace core {
    namespace base {

        void Thread::usleep(const long &microseconds) {
            if (microseconds > 0) {
                wrapper::ConcurrencyFactory::usleep(microseconds);
            }
        }

    }
} // core::base

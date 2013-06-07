/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Thread.h"
#include "core/wrapper/ConcurrencyFactory.h"

namespace core {
    namespace base {

        void Thread::usleep(const long &microseconds) {
            if (microseconds > 0) {
                wrapper::ConcurrencyFactory::getInstance().usleep(microseconds);
            }
        }

    }
} // core::base

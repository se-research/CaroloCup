/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>
#include <ctime>

#include "core/wrapper/POSIX/POSIXConcurrencyFactory.h"
#include "core/wrapper/POSIX/POSIXThread.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXConcurrencyFactory::POSIXConcurrencyFactory() {}

            POSIXConcurrencyFactory::~POSIXConcurrencyFactory() {}

            Thread* POSIXConcurrencyFactory::createThread(Runnable &runnable) {
                return new POSIXThread(runnable);
            }

            void POSIXConcurrencyFactory::usleep(const long &microseconds) {
                struct timespec delay;
                delay.tv_sec = (int)(microseconds / 1000000L);
                delay.tv_nsec = (microseconds % 1000000L) * 1000;
                nanosleep(&delay, NULL);
            }

        }
    }
} // core::wrapper::POSIX

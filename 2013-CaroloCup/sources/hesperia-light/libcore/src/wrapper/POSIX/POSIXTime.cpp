/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sys/time.h>

#include <ctime>

#include "core/wrapper/POSIX/POSIXTime.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXTime::POSIXTime() :
                    m_seconds(0),
                    m_partialMicroseconds(0) {
                struct timeval t;
                gettimeofday(&t, NULL);
                m_seconds = t.tv_sec;
                m_partialMicroseconds = t.tv_usec;
            }

            POSIXTime::~POSIXTime() {}

            int32_t POSIXTime::getSeconds() const {
                return m_seconds;
            }

            int32_t POSIXTime::getPartialMicroseconds() const {
                return m_partialMicroseconds;
            }

        }
    }
} // core::wrapper::POSIX

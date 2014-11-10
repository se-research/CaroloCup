/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

// Using c++11 standard.
#include <chrono>

#include "core/wrapper/WIN32/WIN32Time.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            WIN32Time::WIN32Time() :
                m_seconds(0),
                m_partialMicroseconds(0) {
                std::chrono::time_point<std::chrono::system_clock> t(std::chrono::system_clock::now());
                auto duration = t.time_since_epoch();

                typedef std::chrono::duration<int32_t> seconds_type;
				typedef std::chrono::duration<int64_t, std::micro> microseconds_type;

                seconds_type s = std::chrono::duration_cast<seconds_type>(duration);
                microseconds_type us = std::chrono::duration_cast<microseconds_type>(duration);

                microseconds_type partial_us = us - std::chrono::duration_cast<microseconds_type>(s); // The seconds are converted to microseconds and subtracted from the microseconds representation of duration. Thus, we end up with the same behavior as gettimeofday.

                // The following calculations ensure identical behavior to the gettimeofday call.
                m_seconds = s.count();
                m_partialMicroseconds = partial_us.count();
            }

            WIN32Time::~WIN32Time() {}

            int32_t WIN32Time::getSeconds() const {
                return m_seconds;
            }

            int32_t WIN32Time::getPartialMicroseconds() const {
                return m_partialMicroseconds;
            }

        }
    }
} // core::wrapper::WIN32Impl

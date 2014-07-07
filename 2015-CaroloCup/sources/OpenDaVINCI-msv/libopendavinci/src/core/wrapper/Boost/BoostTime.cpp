/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <boost/date_time/posix_time/posix_time_types.hpp>

#include "core/wrapper/Boost/BoostTime.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            BoostTime::BoostTime() :
                    m_seconds(0),
                    m_partialMicroseconds(0) {
                // Stamp now using January 1, 1970 and actual time.
                boost::posix_time::ptime referenceTime(boost::gregorian::date(1970, boost::gregorian::Jan, 1));
                boost::posix_time::ptime now = boost::posix_time::microsec_clock::local_time();
                boost::posix_time::time_duration timeOfDay = now - referenceTime;

                m_seconds = static_cast<int>(timeOfDay.total_seconds());

                // TODO: Check, if timeOfDay.fractional_seconds() returns a value of type microseconds!
                m_partialMicroseconds = static_cast<int>(timeOfDay.fractional_seconds());
            }

            BoostTime::~BoostTime() {}

            int32_t BoostTime::getSeconds() const {
                return m_seconds;
            }

            int32_t BoostTime::getPartialMicroseconds() const {
                return m_partialMicroseconds;
            }

        }
    }
} // core::wrapper::Boost

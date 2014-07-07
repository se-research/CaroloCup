/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "context/base/ControlledTime.h"

namespace context {
    namespace base {

        using namespace std;

        ControlledTime::ControlledTime() :
            m_seconds(0),
            m_partialMicroseconds(0) {}

        ControlledTime::ControlledTime(const uint32_t &s, const uint32_t &ps) :
            m_seconds(s),
            m_partialMicroseconds(ps) {}

        ControlledTime::ControlledTime(const ControlledTime &ct) :
            m_seconds(ct.getSeconds()),
            m_partialMicroseconds(ct.getPartialMicroseconds()) {}

        ControlledTime::~ControlledTime() {}

        ControlledTime& ControlledTime::operator=(const ControlledTime &ct) {
            setSeconds(ct.getSeconds());
            setPartialMicroseconds(ct.getPartialMicroseconds());

            return (*this);
        }

        int32_t ControlledTime::getSeconds() const {
            return m_seconds;
        }

        int32_t ControlledTime::getPartialMicroseconds() const {
            return m_partialMicroseconds;
        }

        void ControlledTime::setSeconds(const int32_t &s) {
            m_seconds = s;
        }

        void ControlledTime::setPartialMicroseconds(const int32_t &partialMS) {
            m_partialMicroseconds = partialMS;
        }

    }
} // context::base

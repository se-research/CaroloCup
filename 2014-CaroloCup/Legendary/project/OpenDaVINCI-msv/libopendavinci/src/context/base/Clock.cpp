/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "context/base/Clock.h"

namespace context {
    namespace base {

        using namespace std;

        Clock::Clock() :
            m_theTime() {}

        Clock::Clock(const Clock &obj) :
            m_theTime(obj.now()) {}

        Clock::~Clock() {}

        Clock& Clock::operator=(const Clock &obj) {
            m_theTime = obj.now();

            return (*this);
        }

        const ControlledTime Clock::now() const {
            return m_theTime;
        }

        void Clock::increment(const uint32_t &ms) {
            uint32_t newMicroseconds = m_theTime.getPartialMicroseconds() + ms*1000;
            uint32_t additionalSeconds = 0;
            const uint32_t ONESECONDINMICROSECONDS = 1 * 1000 * 1000;
            while (newMicroseconds >= ONESECONDINMICROSECONDS) {
                additionalSeconds++;
                newMicroseconds -= ONESECONDINMICROSECONDS;
            }

            m_theTime.setPartialMicroseconds(newMicroseconds);
            m_theTime.setSeconds(m_theTime.getSeconds() + additionalSeconds);
        }

    }
} // context::base

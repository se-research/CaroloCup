/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "Clock.h"

namespace rec2video {

    using namespace std;

    Clock::Clock() :
        m_millisecondsSinceStart(0),
        m_fractionalMicroseconds(0) {}

    Clock::~Clock() {}

    int32_t Clock::getMillisecondsSinceStart() const {
        return m_millisecondsSinceStart;
    }

    int32_t Clock::getFractionalMicroseconds() const {
        return m_fractionalMicroseconds;
    }

    void Clock::incrementByOneMicrosecond() {
        m_fractionalMicroseconds++;
        if (m_fractionalMicroseconds == 1000) {
            m_millisecondsSinceStart++;
            m_fractionalMicroseconds = 0;
        }
    }

} // rec2video

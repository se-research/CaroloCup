/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_CLOCKTESTSUITE_H_
#define CONTEXT_CLOCKTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>

#include "core/macros.h"
#include "context/base/Clock.h"
#include "context/base/ControlledTime.h"

using namespace std;
using namespace context::base;

class ClockTest : public CxxTest::TestSuite {
    public:
        void testClock() {
            Clock clock;

            ControlledTime time = clock.now();

            TS_ASSERT(time.getSeconds() == 0);
            TS_ASSERT(time.getPartialMicroseconds() == 0);

            // Add 1 ms.
            clock.increment(1);

            time = clock.now();
            TS_ASSERT(time.getSeconds() == 0);
            TS_ASSERT(time.getPartialMicroseconds() == 1000);

            // Add 1 ms.
            clock.increment(1);

            time = clock.now();
            TS_ASSERT(time.getSeconds() == 0);
            TS_ASSERT(time.getPartialMicroseconds() == 2000);

            // Add 997 ms.
            clock.increment(997);

            time = clock.now();
            TS_ASSERT(time.getSeconds() == 0);
            TS_ASSERT(time.getPartialMicroseconds() == 999000);

            // Add 1 ms.
            clock.increment(1);

            time = clock.now();
            TS_ASSERT(time.getSeconds() == 1);
            TS_ASSERT(time.getPartialMicroseconds() == 0);

            // Add 2500 ms.
            clock.increment(2500);

            time = clock.now();
            TS_ASSERT(time.getSeconds() == 3);
            TS_ASSERT(time.getPartialMicroseconds() == 500000);
        }

        void testCopyClock() {
            Clock clock1;
            clock1.increment(12300);

            Clock clock2(clock1);
            Clock clock3 = clock2;

            TS_ASSERT(clock1.now().getSeconds() == clock2.now().getSeconds());
            TS_ASSERT(clock1.now().getPartialMicroseconds() == clock2.now().getPartialMicroseconds());

            TS_ASSERT(clock3.now().getSeconds() == clock2.now().getSeconds());
            TS_ASSERT(clock3.now().getPartialMicroseconds() == clock2.now().getPartialMicroseconds());
        }
};

#endif /*CONTEXT_CLOCKTESTSUITE_H_*/

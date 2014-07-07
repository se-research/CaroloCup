/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_RUNNERTESTSUITE_H_
#define CONTEXT_RUNNERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include "context/base/ControlledTime.h"
#include "context/base/Runner.h"

using namespace std;

using namespace context::base;

class RunnerTestApp : public Runner {
    public:
        RunnerTestApp(const float &freq) :
            m_freq(freq) {}

        virtual float getFrequency() const {
            return m_freq;
        }

    private:
        const float m_freq;
};

class RunnerTest : public CxxTest::TestSuite {
    public:
        void testFrequencies() {
            RunnerTestApp r1(1);
            TS_ASSERT(r1.needsExecution(ControlledTime(0, 0)));
            TS_ASSERT(r1.needsExecution(ControlledTime(1, 0)));
            TS_ASSERT(!r1.needsExecution(ControlledTime(1, 999999)));

            RunnerTestApp r2(2);
            TS_ASSERT(r2.needsExecution(ControlledTime(0, 0)));
            TS_ASSERT(r2.needsExecution(ControlledTime(1, 0)));
            TS_ASSERT(r2.needsExecution(ControlledTime(1, 500000)));
            TS_ASSERT(!r2.needsExecution(ControlledTime(1, 999999)));

            RunnerTestApp r3(0.5);
            TS_ASSERT(r3.needsExecution(ControlledTime(0, 0)));
            TS_ASSERT(!r3.needsExecution(ControlledTime(1, 0)));
            TS_ASSERT(!r3.needsExecution(ControlledTime(1, 500000)));
            TS_ASSERT(!r3.needsExecution(ControlledTime(1, 999999)));
            TS_ASSERT(r3.needsExecution(ControlledTime(2, 0)));
        }
};

#endif /*CONTEXT_RUNNERTESTSUITE_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_MUTEXTESTSUITE_H_
#define CORE_MUTEXTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>

#include "core/base/Mutex.h"

using namespace std;
using namespace core::base;

class MutexTestDirectLocking : public Mutex {
    public:
        void doLock() {
            Mutex::lock();
        }

        bool doTryLock() {
            return Mutex::tryLock();
        }

        void doUnlock() {
            Mutex::unlock();
        }
};

class MutexTest : public CxxTest::TestSuite {
    public:
        void testTryLock() {
            MutexTestDirectLocking m;
            TS_ASSERT(m.doTryLock());
            TS_ASSERT(!m.doTryLock());
        }

        void testLockAndUnlock() {
            MutexTestDirectLocking m2;
            m2.doLock();
            m2.doUnlock();

            m2.doLock();
            TS_ASSERT(!m2.doTryLock());
            m2.doUnlock();
        }
};

#endif /*CORE_MUTEXTESTSUITE_H_*/

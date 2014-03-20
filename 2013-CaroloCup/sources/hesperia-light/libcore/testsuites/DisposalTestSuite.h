/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_DISPOSALTESTSUITE_H_
#define CORE_DISPOSALTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include "core/base/Thread.h"
#include "core/wrapper/Disposable.h"
#include "core/wrapper/DisposalService.h"

using namespace std;

class DisposalTestTrash : public core::wrapper::Disposable {
    public:
        DisposalTestTrash() :
            data(42) {};

        int32_t data;
};

class DisposalTest : public CxxTest::TestSuite {
    public:
        void testDisposal() {
            DisposalTestTrash *trash1regular = new DisposalTestTrash();
            trash1regular->data = 13;
            DisposalTestTrash *trash2final = new DisposalTestTrash();
            trash2final->data = 14;

            TS_ASSERT(trash1regular != NULL);
            TS_ASSERT(trash1regular->data == 13);
            TS_ASSERT(trash2final != NULL);
            TS_ASSERT(trash2final->data == 14);
            TS_ASSERT(trash1regular != trash2final);

            {
                core::wrapper::DisposalService::getInstance().addDisposableForRegularRemoval(trash1regular);
                core::wrapper::DisposalService::getInstance().addDisposableForFinalRemoval(trash2final);

                core::base::Thread::usleep(1000*1000);

//                TS_ASSERT(trash1regular == NULL);
                TS_ASSERT(trash2final != NULL);
            }
//            TS_ASSERT(trash2final == NULL);
        }
};

#endif /*CORE_DISPOSALTESTSUITE_H_*/

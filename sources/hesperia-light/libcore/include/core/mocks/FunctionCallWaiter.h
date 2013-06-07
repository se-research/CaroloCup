/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MOCKS_FUNCTIONCALLWAITER_H
#define MOCKS_FUNCTIONCALLWAITER_H


#include "core/base/Condition.h"

namespace mocks {

    /**
     * FunctionCallWaiter is a helper class that eases the implementation
     * of mocks that hava to check if a specific method was called.
     */
    class FunctionCallWaiter
    {
        public:
            FunctionCallWaiter();

            bool wait();

            bool wasCalled();

            void called();

            void reset();

        private:
            bool m_called;
            core::base::Condition m_condition;
    };
}

#endif

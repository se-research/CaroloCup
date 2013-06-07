/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS_FUNCTIONCALLWAITER_H
#define MOCKS_FUNCTIONCALLWAITER_H

#include "core/base/Lock.h"
#include "core/base/Condition.h"

namespace mocks {

    /**
     * FunctionCallWaiter is a helper class that eases the implementation
     * of mocks that hava to check if a specific method was called.
     */
    class FunctionCallWaiter
    {
        public:
            FunctionCallWaiter() :
				m_called(false),
				m_condition()
			{}

            bool wait() {
            	core::base::Lock lock(m_condition);
                if (!m_called) {
                    return m_condition.waitOnSignalWithTimeout(2000);
                }

                return true;
            }

            bool wasCalled() {
            	core::base::Lock lock(m_condition);
                return m_called;
            }

            void called() {
            	core::base::Lock lock(m_condition);
                m_called = true;
                m_condition.wakeAll();
            }

            void reset() {
                core::base::Lock lock(m_condition);
                m_called = false;
            }

        private:
            bool m_called;
            core::base::Condition m_condition;
    };
}

#endif

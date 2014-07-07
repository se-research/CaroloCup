/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/base/Thread.h"
#include "context/base/RunModuleBreakpoint.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;

        RunModuleBreakpoint::RunModuleBreakpoint(BlockableContainerListener &bcl) :
            m_blockableContainerListener(bcl),
            m_reachedMutex(),
            m_reached(false),
            m_continueMutex(),
            m_continue(false) {}

        RunModuleBreakpoint::~RunModuleBreakpoint() {}

        void RunModuleBreakpoint::reached() {
            // Disable sending BEFORE reaching the breakpoint (since RuntimeControl would increment time after reaching the breakpoint).
            m_blockableContainerListener.setNextContainerAllowed(false);

            // Indicate the outer thread that the inner thread has reached its breakpoint.
            {
                Lock l1(m_reachedMutex);
                m_reached = true;
            }

            // Wait for continue.
            while (!hasContinue()) {
                Thread::usleep(1000);
            }

            // Enable sending.
            m_blockableContainerListener.setNextContainerAllowed(true);

            // Consume continue.
            {
                Lock l3(m_continueMutex);
                m_continue = false;
            }
        }

        void RunModuleBreakpoint::setFinallyReaching() {
            Lock l1(m_reachedMutex);
            m_reached = true;
        }

        bool RunModuleBreakpoint::hasReached() const {
            bool retVal = false;
            {
                Lock l(m_reachedMutex);
                retVal = m_reached;
            }
            return retVal;
        }

        bool RunModuleBreakpoint::hasContinue() const {
            bool retVal = false;
            {
                Lock l1(m_continueMutex);
                retVal = m_continue;
            }
            return retVal;
        }

        void RunModuleBreakpoint::continueExecution() {
            // Prepare reached for next execution.
            {
                Lock l1(m_reachedMutex);
                m_reached = false;
            }

            // Continue execution.
            {
                Lock l2(m_continueMutex);
                m_continue = true;
            }
        }

    }
} // context::base

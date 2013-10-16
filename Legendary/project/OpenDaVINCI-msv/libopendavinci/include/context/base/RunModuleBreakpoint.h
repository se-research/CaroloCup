/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_RUNMODULEBREAKPOINT_H_
#define CONTEXT_BASE_RUNMODULEBREAKPOINT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Breakpoint.h"
#include "core/base/Mutex.h"
#include "context/base/BlockableContainerListener.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class provides access to the SystemContext using elsewhere
         * prepared data.
         */
        class OPENDAVINCI_API RunModuleBreakpoint : public core::base::Breakpoint {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                RunModuleBreakpoint(const RunModuleBreakpoint&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                RunModuleBreakpoint& operator=(const RunModuleBreakpoint&);

            public:
                /**
                 * Constructor.
                 *
                 * @param bcl BlockableContainerListener.
                 */
                RunModuleBreakpoint(BlockableContainerListener &bcl);

                virtual ~RunModuleBreakpoint();

                virtual void reached();

                /**
                 * This method returns true if the breakpoint was reached.
                 *
                 * @return true if the breakpoint was reached.
                 */
                bool hasReached() const;

                /**
                 * This method continues the application's execution.
                 */
                void continueExecution();

                /**
                 * This method finally unlocks this breakpoint. This
                 * method is intended to be used by ConferenceClientModuleRunner
                 * to unlock the waiting RuntimeControl!
                 */
                void setFinallyReaching();

            private:
                /**
                 * This method returns true if the application can continue
                 * to execute.
                 *
                 * @return true if continue.
                 */
                bool hasContinue() const;

            private:
                BlockableContainerListener &m_blockableContainerListener;

                mutable core::base::Mutex m_reachedMutex;
                bool m_reached;

                mutable core::base::Mutex m_continueMutex;
                bool m_continue;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_RUNMODULEBREAKPOINT_H_*/

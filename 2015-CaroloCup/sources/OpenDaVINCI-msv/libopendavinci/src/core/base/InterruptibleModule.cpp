/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/InterruptibleModule.h"
#include "core/base/Lock.h"
#include "core/base/Thread.h"

namespace core {
    namespace base {

        using namespace std;
        using namespace core::base;
        using namespace exceptions;

        InterruptibleModule::InterruptibleModule() :
                m_breakpointMutex(),
                m_breakpoint(NULL) {}

        InterruptibleModule::~InterruptibleModule() {}

        void InterruptibleModule::setBreakpoint(Breakpoint *bp) {
            Lock l(m_breakpointMutex);
            m_breakpoint = bp;
        }

        void InterruptibleModule::calledGetModuleState() {
            Breakpoint *bp = NULL;

            // Check if it's necessary to call the breakpoint.
            {
                Lock l(m_breakpointMutex);
                bp = m_breakpoint;
            }

            // Reach breakpoint.
            if (bp != NULL) {
                bp->reached();

                // Yielding other threads to deliver containers.
                Thread::usleep(100);
            }
            else {
                // Yielding other threads and adjust to a fixed frequency if no breakpoint is set.
                wait();
            }
        }

    }
} // core::base

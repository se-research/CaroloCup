/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_INTERRUPTIBLEMODULE_H_
#define OPENDAVINCI_CORE_BASE_INTERRUPTIBLEMODULE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/AbstractModule.h"
#include "core/base/Breakpoint.h"
#include "core/base/Mutex.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is the first derivate of AbstractModule allowing
         * interuption.
         */
        class OPENDAVINCI_API InterruptibleModule : public AbstractModule {
            protected:
                /**
                 * Constructor for any module.
                 */
                InterruptibleModule();

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                InterruptibleModule(const InterruptibleModule&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                InterruptibleModule& operator=(const InterruptibleModule&);

            public:
                virtual ~InterruptibleModule();

                /**
                 * This method sets the breakpoint in getModuleState()
                 * to interrupt regular program execution.
                 *
                 * @param bp Breakpoint to set.
                 */
                void setBreakpoint(Breakpoint *bp);

                /**
                 * This method executes the application body.
                 */
                virtual ModuleState::MODULE_EXITCODE runModule() = 0;

            private:
                virtual void calledGetModuleState();

            private:
                Mutex m_breakpointMutex;
                Breakpoint *m_breakpoint;
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_INTERRUPTIBLEMODULE_H_*/

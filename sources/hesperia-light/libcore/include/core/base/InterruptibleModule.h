/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_INTERRUPTIBLEMODULE_H_
#define HESPERIA_CORE_BASE_INTERRUPTIBLEMODULE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

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
        class HESPERIA_API InterruptibleModule : public AbstractModule {
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

#endif /*HESPERIA_CORE_BASE_INTERRUPTIBLEMODULE_H_*/

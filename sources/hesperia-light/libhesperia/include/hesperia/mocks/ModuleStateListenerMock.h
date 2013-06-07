/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MOCKS__MODULESTATELISTENERMOCK_H_
#define MOCKS__MODULESTATELISTENERMOCK_H_

#include <iostream>

#include "core/native.h"
#include "core/base/Condition.h"
#include "core/base/Lock.h"
#include "core/base/Mutex.h"
#include "core/base/ModuleState.h"

#include "hesperia/dmcp/ModuleStateListener.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"

namespace mocks {

    class ModuleStateListenerMock : public hesperia::dmcp::ModuleStateListener {
        private:
            core::base::ModuleState::MODULE_STATE m_ms;
            core::base::ModuleState::MODULE_EXITCODE m_me;
            bool m_stateCalled;
            bool m_exitCalled;
            core::base::Condition m_stateCondition;
            core::base::Condition m_exitCondition;

        public:
            ModuleStateListenerMock()
                    : m_ms(core::base::ModuleState::UNDEFINED_STATE),
                    m_me(core::base::ModuleState::UNDEFINED_EXITCODE),
                    m_stateCalled(false),
                    m_exitCalled(false),
                    m_stateCondition(),
                    m_exitCondition() {};

            virtual ~ModuleStateListenerMock() {};

            void waitForStateCall() {
                core::base::Lock l(m_stateCondition);

                if (!wasStateCalled() ) {
                    m_stateCondition.waitOnSignalWithTimeout(1000);
                }
            }

            bool wasStateCalled() {
                return m_stateCalled;
            }

            void waitForExitCodeCall() {
                core::base::Lock l(m_exitCondition);

                if (!wasExitCodeCalled() ) {
                    m_exitCondition.waitOnSignalWithTimeout(1000);
                }
            }

            bool wasExitCodeCalled() {
                return m_exitCalled;
            }

            virtual void handleChangeState(const hesperia::data::dmcp::ModuleDescriptor& /*md*/,
                                           const core::base::ModuleState::MODULE_STATE &ms) {
                core::base::Lock l(m_stateCondition);
                m_ms = ms;
                m_stateCalled = true;
                m_stateCondition.wakeAll();
            }

            virtual void handleExitCode(const hesperia::data::dmcp::ModuleDescriptor& /*md*/,
                                        const core::base::ModuleState::MODULE_EXITCODE &me) {
                core::base::Lock l(m_exitCondition);
                m_me = me;
                m_exitCalled = true;
                m_exitCondition.wakeAll();
            }

            void reset() {
                m_ms = core::base::ModuleState::UNDEFINED_STATE;
                m_me = core::base::ModuleState::UNDEFINED_EXITCODE;
                m_stateCalled = false;
                m_exitCalled = false;
            }

            core::base::ModuleState::MODULE_STATE getModuleState() {
                core::base::Lock l(m_stateCondition);
                return m_ms;
            }

            core::base::ModuleState::MODULE_EXITCODE getExitCode() {
                core::base::Lock l(m_exitCondition);
                return m_me;
            }
    };
}
#endif

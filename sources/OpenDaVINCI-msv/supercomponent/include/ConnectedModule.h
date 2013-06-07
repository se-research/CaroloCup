/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SUPERCOMPONENT_CONNECTEDMODULE_H_
#define SUPERCOMPONENT_CONNECTEDMODULE_H_

#include "core/base/ModuleState.h"
#include "core/dmcp/connection/ModuleConnection.h"

namespace supercomponent {

    using namespace std;

    class ConnectedModule
    {
        public:
            ConnectedModule(core::dmcp::connection::ModuleConnection* connection,
                            const core::base::ModuleState::MODULE_STATE& moduleState );

            virtual ~ConnectedModule();

            core::dmcp::connection::ModuleConnection& getConnection();

            core::base::ModuleState::MODULE_STATE getState() const;
            void setState(const core::base::ModuleState::MODULE_STATE& state);

            void setExitCode();
            bool hasExitCode() const;

        protected:
            core::base::ModuleState::MODULE_STATE m_state;
            core::dmcp::connection::ModuleConnection* m_connection;
            bool m_hasExitCode;

        private:
            ConnectedModule(const ConnectedModule &);
            ConnectedModule& operator=(const ConnectedModule &);
    };
}

#endif /*SUPERCOMPONENT_CONNECTEDMODULE_H_*/

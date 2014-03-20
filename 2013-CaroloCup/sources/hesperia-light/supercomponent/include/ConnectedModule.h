/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef SUPERCOMPONENT_CONNECTEDMODULE_H_
#define SUPERCOMPONENT_CONNECTEDMODULE_H_

#include "core/base/ModuleState.h"
#include "hesperia/dmcp/connection/ModuleConnection.h"

namespace supercomponent {

    using namespace std;

    class ConnectedModule
    {
        public:
            ConnectedModule(hesperia::dmcp::connection::ModuleConnection* connection,
                            const core::base::ModuleState::MODULE_STATE& moduleState );

            virtual ~ConnectedModule();

            hesperia::dmcp::connection::ModuleConnection& getConnection();

            core::base::ModuleState::MODULE_STATE getState() const;
            void setState(const core::base::ModuleState::MODULE_STATE& state);

            void setExitCode();
            bool hasExitCode() const;

        protected:
            core::base::ModuleState::MODULE_STATE m_state;
            hesperia::dmcp::connection::ModuleConnection* m_connection;
            bool m_hasExitCode;

        private:
            ConnectedModule(const ConnectedModule &);
            ConnectedModule& operator=(const ConnectedModule &);
    };
}

#endif /*SUPERCOMPONENT_CONNECTEDMODULE_H_*/

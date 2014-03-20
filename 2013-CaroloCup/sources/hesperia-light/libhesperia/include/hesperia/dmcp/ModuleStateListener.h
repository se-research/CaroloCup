/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_MODULESTATELISTENER_H_
#define HESPERIA_DMCP_MODULESTATELISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/ModuleState.h"
#include "core/data/Container.h"
#include "core/data/RuntimeStatistic.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"


namespace hesperia {
    namespace dmcp {

        class HESPERIA_API ModuleStateListener {
            public:
                virtual ~ModuleStateListener() {};

                virtual void handleChangeState(const hesperia::data::dmcp::ModuleDescriptor& md,
                                               const core::base::ModuleState::MODULE_STATE &ms) = 0;

                virtual void handleExitCode(const hesperia::data::dmcp::ModuleDescriptor& md,
                                            const core::base::ModuleState::MODULE_EXITCODE &me) = 0;

                virtual void handleRuntimeStatistics(const hesperia::data::dmcp::ModuleDescriptor& md,
                                                     const core::data::RuntimeStatistic& rs) = 0;

                virtual void handleConnectionLost(const hesperia::data::dmcp::ModuleDescriptor& md) = 0;

                virtual void handleUnkownContainer(const hesperia::data::dmcp::ModuleDescriptor& md,
                                                   const core::data::Container& container) = 0;
        };
    }
} // hesperia::dmcp

#endif /* HESPERIA_DMCP_MODULESTATELISTENER_H_ */

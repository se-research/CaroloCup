/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_MODULESTATELISTENER_H_
#define OPENDAVINCI_DMCP_MODULESTATELISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/ModuleState.h"
#include "core/data/Container.h"
#include "core/data/RuntimeStatistic.h"
#include "core/data/dmcp/ModuleDescriptor.h"

namespace core {
    namespace dmcp {

        class OPENDAVINCI_API ModuleStateListener {
            public:
                virtual ~ModuleStateListener() {};

                virtual void handleChangeState(const core::data::dmcp::ModuleDescriptor& md,
                                               const core::base::ModuleState::MODULE_STATE &ms) = 0;

                virtual void handleExitCode(const core::data::dmcp::ModuleDescriptor& md,
                                            const core::base::ModuleState::MODULE_EXITCODE &me) = 0;

                virtual void handleRuntimeStatistics(const core::data::dmcp::ModuleDescriptor& md,
                                                     const core::data::RuntimeStatistic& rs) = 0;

                virtual void handleConnectionLost(const core::data::dmcp::ModuleDescriptor& md) = 0;

                virtual void handleUnkownContainer(const core::data::dmcp::ModuleDescriptor& md,
                                                   const core::data::Container& container) = 0;
        };
    }
} // core::dmcp

#endif /* OPENDAVINCI_DMCP_MODULESTATELISTENER_H_ */

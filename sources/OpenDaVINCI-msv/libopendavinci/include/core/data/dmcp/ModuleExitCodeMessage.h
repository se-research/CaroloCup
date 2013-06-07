/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_DMCP_MODULEEXITCODEMESSAGE_H_
#define OPENDAVINCI_DATA_DMCP_MODULEEXITCODEMESSAGE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"
#include "core/base/ModuleState.h"

namespace core {
    namespace data {
        namespace dmcp {

            using namespace std;

            class OPENDAVINCI_API ModuleExitCodeMessage : public core::data::SerializableData {
                public:
                    ModuleExitCodeMessage();
                    ModuleExitCodeMessage(const core::base::ModuleState::MODULE_EXITCODE &me);

                    virtual ~ModuleExitCodeMessage();

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;
                    core::base::ModuleState::MODULE_EXITCODE getModuleExitCode() const;

                private:
                    core::base::ModuleState::MODULE_EXITCODE m_me;
            };
        }
    }
} // core::data::dmcp

#endif /*OPENDAVINCI_DATA_DMCP_MODULESTATEMESSAGE_H_*/

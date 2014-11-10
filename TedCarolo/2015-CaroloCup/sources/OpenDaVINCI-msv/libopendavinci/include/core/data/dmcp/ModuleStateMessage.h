/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_DMCP_MODULESTATEMESSAGE_H_
#define OPENDAVINCI_DATA_DMCP_MODULESTATEMESSAGE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"
#include "core/base/ModuleState.h"

namespace core {
    namespace data {
        namespace dmcp {

            using namespace std;

            class OPENDAVINCI_API ModuleStateMessage : public core::data::SerializableData {
                public:
                    ModuleStateMessage();
                    ModuleStateMessage(const core::base::ModuleState::MODULE_STATE &ms);

                    virtual ~ModuleStateMessage();

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;
                    core::base::ModuleState::MODULE_STATE getModuleState() const;

                private:
                    core::base::ModuleState::MODULE_STATE m_ms;
            };
        }
    }
} // core::data::dmcp

#endif /*OPENDAVINCI_DATA_DMCP_MODULESTATEMESSAGE_H_*/

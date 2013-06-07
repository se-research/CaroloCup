/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_DMCP_MODULESTATEMESSAGE_H_
#define HESPERIA_DATA_DMCP_MODULESTATEMESSAGE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/base/ModuleState.h"

namespace hesperia {
    namespace data {
        namespace dmcp {

            using namespace std;

            class HESPERIA_API ModuleStateMessage : public core::data::SerializableData {
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
} // hesperia::data::dmcp

#endif /*HESPERIA_DATA_DMCP_MODULESTATEMESSAGE_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_DMCP_MODULEEXITCODEMESSAGE_H_
#define HESPERIA_DATA_DMCP_MODULEEXITCODEMESSAGE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/base/ModuleState.h"

namespace hesperia {
    namespace data {
        namespace dmcp {

            using namespace std;

            class HESPERIA_API ModuleExitCodeMessage : public core::data::SerializableData {
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
} // hesperia::data::dmcp

#endif /*HESPERIA_DATA_DMCP_MODULESTATEMESSAGE_H_*/

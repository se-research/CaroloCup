/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef SUPERCOMPONENT_CONNECTEDMODULES_H_
#define SUPERCOMPONENT_CONNECTEDMODULES_H_

#include "core/base/ModuleState.h"
#include "core/base/Mutex.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"
#include "hesperia/data/dmcp/ModuleDescriptorComparator.h"

#include "ConnectedModule.h"

namespace supercomponent {

    using namespace std;

    class ConnectedModules
    {
        public:
            ConnectedModules();
            virtual ~ConnectedModules();

            void addModule(const hesperia::data::dmcp::ModuleDescriptor& md, ConnectedModule* module);
            ConnectedModule* getModule(const hesperia::data::dmcp::ModuleDescriptor& md);
            void removeModule(const hesperia::data::dmcp::ModuleDescriptor& md);
            bool hasModule(const hesperia::data::dmcp::ModuleDescriptor& md);

            void deleteAllModules();

        protected:
            core::base::Mutex m_modulesMutex;
            map< hesperia::data::dmcp::ModuleDescriptor,
                 ConnectedModule*,
                 hesperia::data::dmcp::ModuleDescriptorComparator> m_modules;

        private:
            ConnectedModules(const ConnectedModule &);
            ConnectedModules& operator=(const ConnectedModule &);
    };
}

#endif /*SUPERCOMPONENT_CONNECTEDMODULES_H_*/

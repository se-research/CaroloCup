/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SUPERCOMPONENT_CONNECTEDMODULES_H_
#define SUPERCOMPONENT_CONNECTEDMODULES_H_

#include "core/base/ModuleState.h"
#include "core/base/Mutex.h"
#include "core/data/dmcp/PulseMessage.h"
#include "core/data/dmcp/ModuleDescriptor.h"
#include "core/data/dmcp/ModuleDescriptorComparator.h"

#include "ConnectedModule.h"

namespace supercomponent {

    using namespace std;

    class ConnectedModules {
        public:
            ConnectedModules();
            virtual ~ConnectedModules();

            void addModule(const core::data::dmcp::ModuleDescriptor& md, ConnectedModule* module);
            ConnectedModule* getModule(const core::data::dmcp::ModuleDescriptor& md);
            void removeModule(const core::data::dmcp::ModuleDescriptor& md);
            bool hasModule(const core::data::dmcp::ModuleDescriptor& md);

            /**
             * This method sends a pulse to all connected modules.
             *
             * @param pm Pulse to be sent.
             */
            void pulse(const core::data::dmcp::PulseMessage &pm);

            /**
             * This method sends a pulse to all connected modules but shifts
             * the alignment interval by shift microseconds for each connected
             * module.
             *
             * @param pm Pulse to be sent.
             * @shift Increment for each newly connected module.
             */
            void pulseShift(const core::data::dmcp::PulseMessage &pm, const uint32_t &shift);

            void deleteAllModules();

        protected:
            core::base::Mutex m_modulesMutex;
            map< core::data::dmcp::ModuleDescriptor,
                 ConnectedModule*,
                 core::data::dmcp::ModuleDescriptorComparator> m_modules;

        private:
            ConnectedModules(const ConnectedModule &);
            ConnectedModules& operator=(const ConnectedModule &);
    };
}

#endif /*SUPERCOMPONENT_CONNECTEDMODULES_H_*/

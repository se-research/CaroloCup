/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "ConnectedModules.h"

#include "core/base/Lock.h"

namespace supercomponent {

    using namespace std;
    using namespace core::base;
    using namespace core::data::dmcp;

    ConnectedModules::ConnectedModules() :
        m_modulesMutex(),
        m_modules()
    {}

    ConnectedModules::~ConnectedModules() {
        deleteAllModules();
    }

    void ConnectedModules::addModule(const ModuleDescriptor& md, ConnectedModule* module) {
        Lock l(m_modulesMutex);
        m_modules[md] = module;
    }

    ConnectedModule* ConnectedModules::getModule(const ModuleDescriptor& md) {
        Lock l(m_modulesMutex);
        return m_modules[md];
    }

    void ConnectedModules::removeModule(const ModuleDescriptor& md) {
        Lock l(m_modulesMutex);
        m_modules.erase(md);
    }

    bool ConnectedModules::hasModule(const ModuleDescriptor& md) {
        Lock l(m_modulesMutex);
        return (m_modules.count(md) != 0);
    }

    void ConnectedModules::pulse(const core::data::dmcp::PulseMessage &pm) {
        Lock l(m_modulesMutex);
        map< core::data::dmcp::ModuleDescriptor,
             ConnectedModule*,
             core::data::dmcp::ModuleDescriptorComparator>::iterator iter;

        for (iter = m_modules.begin(); iter != m_modules.end(); ++iter) {
            iter->second->getConnection().pulse(pm);
        }
    }

    void ConnectedModules::pulseShift(const core::data::dmcp::PulseMessage &pm, const uint32_t &shift) {
        Lock l(m_modulesMutex);
        map< core::data::dmcp::ModuleDescriptor,
             ConnectedModule*,
             core::data::dmcp::ModuleDescriptorComparator>::iterator iter;

        uint32_t connectedModulesCounter = 0;
        core::data::dmcp::PulseMessage pm_shifted = pm;
        const core::data::TimeStamp pm_org_ts = pm.getRealtimeFromSupercomponent();

        for (iter = m_modules.begin(); iter != m_modules.end(); ++iter) {
            core::data::TimeStamp ts(0, shift * connectedModulesCounter);
            core::data::TimeStamp shiftedTime = pm_org_ts + ts;

            pm_shifted.setRealTimeFromSupercomponent(shiftedTime);
            iter->second->getConnection().pulse(pm_shifted);

            connectedModulesCounter++;
        }
    }

    void ConnectedModules::deleteAllModules() {
        Lock l(m_modulesMutex);
        map< core::data::dmcp::ModuleDescriptor,
             ConnectedModule*,
             core::data::dmcp::ModuleDescriptorComparator>::iterator iter;

        for (iter = m_modules.begin(); iter != m_modules.end(); ++iter) {
            iter->second->getConnection().setModuleStateListener(NULL);
            delete iter->second;
        }

        m_modules.clear();
    }

}

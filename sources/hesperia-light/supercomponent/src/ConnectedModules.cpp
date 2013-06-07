/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "ConnectedModules.h"

#include "core/base/Lock.h"

namespace supercomponent {

    using namespace std;
    using namespace core::base;
    using namespace hesperia::data::dmcp;

    ConnectedModules::ConnectedModules() :
        m_modulesMutex(),
        m_modules()
    {}

    ConnectedModules::~ConnectedModules() {
        deleteAllModules();
    }

    void ConnectedModules::addModule(const ModuleDescriptor& md, ConnectedModule* module)
    {
        Lock l(m_modulesMutex);
        m_modules[md] = module;
    }

    ConnectedModule* ConnectedModules::getModule(const ModuleDescriptor& md)
    {
        Lock l(m_modulesMutex);
        return m_modules[md];
    }

    void ConnectedModules::removeModule(const ModuleDescriptor& md)
    {
        Lock l(m_modulesMutex);
        m_modules.erase(md);
    }

    bool ConnectedModules::hasModule(const ModuleDescriptor& md)
    {
        Lock l(m_modulesMutex);
        return (m_modules.count(md) != 0);
    }

    void ConnectedModules::deleteAllModules()
    {
        Lock l(m_modulesMutex);
        map< hesperia::data::dmcp::ModuleDescriptor,
             ConnectedModule*,
             hesperia::data::dmcp::ModuleDescriptorComparator>::iterator iter;

        for (iter = m_modules.begin(); iter != m_modules.end(); ++iter) {
            iter->second->getConnection().setModuleStateListener(NULL);
            delete iter->second;
        }

        m_modules.clear();
    }

}

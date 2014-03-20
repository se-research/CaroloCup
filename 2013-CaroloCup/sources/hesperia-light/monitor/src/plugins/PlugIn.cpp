/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Lock.h"

#include "plugins/PlugIn.h"

namespace plugins {

    using namespace std;
    using namespace core::base;
    using namespace monitor;

    PlugIn::PlugIn(const string &name, const KeyValueConfiguration &kvc, QWidget* prnt) :
            m_parent(prnt),
            m_name(name),
            m_description(),
            m_kvc(kvc),
            m_containerObserverMutex(),
            m_containerObserver() {}

    PlugIn::~PlugIn() {}

    ContainerObserver* PlugIn::getContainerObserver() const {
        Lock l(m_containerObserverMutex);
        return m_containerObserver;
    }

    void PlugIn::setContainerObserver(ContainerObserver *containerObserver) {
        Lock l(m_containerObserverMutex);
        m_containerObserver = containerObserver;
    }

    const string PlugIn::getName() const {
        return m_name;
    }

    void PlugIn::setDescription(const string &description) {
      m_description = description;
    }

    const string PlugIn::getDescription() const {
      return m_description;
    }

    QWidget* PlugIn::getParentQWidget() {
        return m_parent;
    }

    const KeyValueConfiguration PlugIn::getKeyValueConfiguration() const {
        return m_kvc;
    }

} // plugins

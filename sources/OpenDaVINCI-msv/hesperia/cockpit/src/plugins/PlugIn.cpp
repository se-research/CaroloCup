/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/Lock.h"

#include "plugins/PlugIn.h"

namespace cockpit {

    namespace plugins {

        using namespace std;
        using namespace core::base;

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

} // cockpit


/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/io/ContainerConference.h"

namespace core {
    namespace io {

        using namespace std;
        using namespace base;
        using namespace data;

        ContainerConference::ContainerConference() :
                m_containerListenerMutex(),
                m_containerListener(NULL) {}

        ContainerConference::~ContainerConference() {}

        void ContainerConference::setContainerListener(ContainerListener *cl) {
            Lock l(m_containerListenerMutex);
            m_containerListener = cl;
        }

        bool ContainerConference::hasContainerListener() const {
            bool hasListener = false;
            {
                Lock l(m_containerListenerMutex);
                hasListener = (m_containerListener != NULL);
            }
            return hasListener;
        }

        void ContainerConference::receive(Container &c) {
            Lock l(m_containerListenerMutex);
            if (m_containerListener != NULL) {
                m_containerListener->nextContainer(c);
            }
        }

    }
} // core::io

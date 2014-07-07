/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "context/base/ContainerDeliverer.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;
        using namespace core::data;
        using namespace core::io;

        ContainerDeliverer::ContainerDeliverer() :
            m_containerListenerMutex(),
            m_containerListener(NULL) {}

        ContainerDeliverer::~ContainerDeliverer() {}

        void ContainerDeliverer::setContainerListener(ContainerListener *cl) {
            Lock l(m_containerListenerMutex);
            m_containerListener = cl;
        }

        void ContainerDeliverer::nextContainer(Container &c) {
            Lock l(m_containerListenerMutex);
            if (m_containerListener != NULL) {
                m_containerListener->nextContainer(c);
            }
        }

    }
} // context::base

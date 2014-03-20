/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/io/ContainerConferenceFactory.h"
#include "core/io/UDPMultiCastContainerConference.h"

namespace core {
    namespace io {

        using namespace std;
        using namespace base;

        // Initialize singleton instance.
        Mutex ContainerConferenceFactory::m_singletonMutex;
        ContainerConferenceFactory* ContainerConferenceFactory::m_singleton = NULL;

        ContainerConferenceFactory::ContainerConferenceFactory() {}

        ContainerConferenceFactory::~ContainerConferenceFactory() {
            setSingleton(NULL);
        }

        void ContainerConferenceFactory::setSingleton(ContainerConferenceFactory *singleton) {
            ContainerConferenceFactory::m_singleton = singleton;
        }

        ContainerConferenceFactory& ContainerConferenceFactory::getInstance() {
            {
                Lock l(ContainerConferenceFactory::m_singletonMutex);
                if (ContainerConferenceFactory::m_singleton == NULL) {
                    ContainerConferenceFactory::setSingleton(new ContainerConferenceFactory());
                }
            }

            return (*ContainerConferenceFactory::m_singleton);
        }

        ContainerConference* ContainerConferenceFactory::getContainerConference(const string &address, const uint32_t &port) {
            return new UDPMultiCastContainerConference(address, port);
        }

    }
} // core::io

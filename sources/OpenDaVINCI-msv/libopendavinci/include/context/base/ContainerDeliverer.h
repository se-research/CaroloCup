/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_CONTAINERDELIVERER_H_
#define CONTEXT_BASE_CONTAINERDELIVERER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Mutex.h"
#include "core/data/Container.h"
#include "core/io/ContainerListener.h"
#include "core/io/ContainerObserver.h"
#include "core/wrapper/Disposable.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class delivers Containers to ControlledContainerConferenceForSystemUnderTest.
         * Every ControlledContainerConferenceForSystemUnderTest has its own
         * ContainerDeliverer to ensure synchronous delivery.
         */
        class OPENDAVINCI_API ContainerDeliverer : public core::io::ContainerObserver, public core::io::ContainerListener, public core::wrapper::Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ContainerDeliverer(const ContainerDeliverer&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ContainerDeliverer& operator=(const ContainerDeliverer&);

            public:
                ContainerDeliverer();

                virtual ~ContainerDeliverer();

                virtual void setContainerListener(core::io::ContainerListener *cl);

                // This method is called by ControlledContainerConferenceFactory to send c to the registered ContainerListener from an app.
                virtual void nextContainer(core::data::Container &c);

            private:
                core::base::Mutex m_containerListenerMutex;
                core::io::ContainerListener *m_containerListener;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_CONTAINERDELIVERER_H_*/

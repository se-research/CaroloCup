/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_SYSTEMFEEDBACKCOMPONENT_H_
#define CONTEXT_BASE_SYSTEMFEEDBACKCOMPONENT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/wrapper/Time.h"
#include "context/base/SystemContextComponent.h"
#include "context/base/SendContainerToSystemsUnderTest.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class can be used to compute data for system's parts.
         */
        class OPENDAVINCI_API SystemFeedbackComponent : public SystemContextComponent {
            public:
                virtual ~SystemFeedbackComponent();

                /**
                 * This method is called whenever a step for this system's
                 * part must be computed.
                 *
                 * @param t Current system time.
                 * @param sender Sender for containers to be delivered to Systems Under Tests.
                 */
                virtual void step(const core::wrapper::Time &t, SendContainerToSystemsUnderTest &sender) = 0;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_SYSTEMFEEDBACKCOMPONENT_H_*/

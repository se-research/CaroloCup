/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_RUNTIMECONTROLINTERFACE_H_
#define CONTEXT_BASE_RUNTIMECONTROLINTERFACE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/KeyValueConfiguration.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This interface provides information for the RuntimeControl.
         */
        class OPENDAVINCI_API RuntimeControlInterface {
            public:
                virtual ~RuntimeControlInterface();

                /**
                 * This method returns the configuration to be used for the
                 * SystemContext.
                 *
                 * @return KeyValueConfiguration to be used.
                 */
                virtual const core::base::KeyValueConfiguration getConfiguration() const = 0;

                /**
                 * This method returns the multicast group to be used.
                 *
                 * @return Multicast group.
                 */
                virtual const string getMulticastGroup() const = 0;

                /**
                 * This method returns the conference ID (CID).
                 *
                 * @return CID.
                 */
                virtual uint32_t getCID() const = 0;

                /**
                 * This method returns true if the RuntimeControl should
                 * be verbose.
                 *
                 * @return true if the RuntimeControl should be verbose.
                 */
                virtual bool isVerbose() const = 0;

                /**
                 * This method returns true if the RuntimeControl should
                 * provide supercomponent-functionality.
                 *
                 * @return true if the RuntimeControl should provide supercomponent functionality.
                 */
                virtual bool isSupercomponent() const = 0;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_RUNTIMECONTROLINTERFACE_H_*/

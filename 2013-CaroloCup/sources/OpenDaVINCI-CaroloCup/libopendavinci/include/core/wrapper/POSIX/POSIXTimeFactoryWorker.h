/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTIMEFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTIMEFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TimeFactoryWorker.h"
#include "core/wrapper/POSIX/POSIXTime.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API TimeFactoryWorker<SystemLibraryPosix>
        {
            public:
                class Time* now()
                {
                    return new POSIX::POSIXTime();
                }
        };
    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTIMEFACTORYWORKER_H_*/

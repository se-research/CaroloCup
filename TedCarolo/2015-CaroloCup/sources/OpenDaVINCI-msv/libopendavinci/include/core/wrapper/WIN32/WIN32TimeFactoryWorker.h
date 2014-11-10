/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TIMEFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TIMEFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TimeFactoryWorker.h"
#include "core/wrapper/WIN32/WIN32Time.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API TimeFactoryWorker<SystemLibraryWin32>
        {
            public:
                class Time* now()
                {
                    return new WIN32Impl::WIN32Time();
                }
        };
    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TIMEFACTORYWORKER_H_*/

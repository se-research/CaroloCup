/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTIMEFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTIMEFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TimeFactoryWorker.h"
#include "core/wrapper/Boost/BoostTime.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API TimeFactoryWorker<SystemLibraryBoost>
        {
            public:
                static Time* now()
                {
                    return new Boost::BoostTime();
                }
        };

    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTIMEFACTORYWORKER_H_*/

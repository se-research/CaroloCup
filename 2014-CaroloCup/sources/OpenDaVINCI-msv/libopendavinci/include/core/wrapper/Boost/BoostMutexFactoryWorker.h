/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTMUTEXFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTMUTEXFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/MutexFactoryWorker.h"

#include "core/wrapper/Boost/BoostMutex.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API MutexFactoryWorker<SystemLibraryBoost>
        {
            public:
                static Mutex* createMutex()
                {
                    return new core::wrapper::Boost::BoostMutex();
                }
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTMUTEXFACTORY_H_*/

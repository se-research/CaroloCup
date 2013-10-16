/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSHAREDMEMORYFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSHAREDMEMORYFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SharedMemoryFactoryWorker.h"
#include "core/wrapper/Boost/BoostSharedMemory.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API SharedMemoryFactoryWorker<SystemLibraryBoost>
        {
            public:
                static SharedPointer<SharedMemory> createSharedMemory(const string &name, const uint32_t &size)
                {
                    return SharedPointer<SharedMemory>(new Boost::BoostSharedMemory(name, size));
                };

                static SharedPointer<SharedMemory> attachToSharedMemory(const string &name)
                {
                    return SharedPointer<SharedMemory>(new Boost::BoostSharedMemory(name));
                };
        };
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSHAREDMEMORYFACTORYWORKER_H_*/

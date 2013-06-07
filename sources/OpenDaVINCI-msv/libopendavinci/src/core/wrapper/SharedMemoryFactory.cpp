/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/SharedMemoryFactory.h"

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"

#include "core/wrapper/SharedMemoryFactoryWorker.h"
#include "core/wrapper/SystemLibraryProducts.h"

#ifdef HAVE_BOOST_LIBRARIES
    #include "core/wrapper/Boost/BoostSharedMemoryFactoryWorker.h"
#endif
#ifndef WIN32
    #include "core/wrapper/POSIX/POSIXSharedMemoryFactoryWorker.h"
#endif

namespace core {
    namespace wrapper {

        SharedPointer<SharedMemory> SharedMemoryFactory::createSharedMemory(const string &name, const uint32_t &size)
        {
            typedef ConfigurationTraits<SystemLibraryProducts>::configuration configuration;

            return SharedMemoryFactoryWorker<configuration::value>::createSharedMemory(name, size);
        }

        SharedPointer<SharedMemory> SharedMemoryFactory::attachToSharedMemory(const string &name)
        {
            typedef ConfigurationTraits<SystemLibraryProducts>::configuration configuration;

            return SharedMemoryFactoryWorker<configuration::value>::attachToSharedMemory(name);
        }
    }
} // core::wrapper

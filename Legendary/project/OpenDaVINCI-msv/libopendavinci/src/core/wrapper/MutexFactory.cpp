/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"
#include "core/wrapper/SystemLibraryProducts.h"
#include "core/wrapper/MutexFactoryWorker.h"

#ifdef HAVE_BOOST_LIBRARIES
    #include "core/wrapper/Boost/BoostMutexFactoryWorker.h"
#endif
#ifndef WIN32
    #include "core/wrapper/POSIX/POSIXMutexFactoryWorker.h"
#endif

namespace core {
    namespace wrapper {

        Mutex* MutexFactory::createMutex()
        {
            typedef ConfigurationTraits<SystemLibraryProducts>::configuration configuration;

            return MutexFactoryWorker<configuration::value>::createMutex();
        }

      }
} // core::wrapper


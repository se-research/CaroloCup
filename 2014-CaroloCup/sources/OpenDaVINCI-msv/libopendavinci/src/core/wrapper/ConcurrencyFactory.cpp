/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/ConcurrencyFactory.h"
#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"
#include "core/wrapper/SystemLibraryProducts.h"
#include "core/wrapper/ConcurrencyFactoryWorker.h"

#ifdef HAVE_BOOST_LIBRARIES
  #include "core/wrapper/Boost/BoostConcurrencyFactoryWorker.h"
#endif
#ifndef WIN32
  #include "core/wrapper/POSIX/POSIXConcurrencyFactoryWorker.h"
#endif

namespace core {
    namespace wrapper {

        Thread* ConcurrencyFactory::createThread(Runnable &runnable)
        {
            typedef ConfigurationTraits<SystemLibraryProducts>::configuration configuration;

            return ConcurrencyFactoryWorker<configuration::value>::createThread(runnable);
        }

        void ConcurrencyFactory::usleep(const long &microseconds)
        {
            typedef ConfigurationTraits<SystemLibraryProducts>::configuration configuration;

            return ConcurrencyFactoryWorker<configuration::value>::usleep(microseconds);
        }
    }
} // core::wrapper

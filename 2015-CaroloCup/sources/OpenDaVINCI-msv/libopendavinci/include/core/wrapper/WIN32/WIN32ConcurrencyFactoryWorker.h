/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32_WIN32CONCURRENCYFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32_WIN32CONCURRENCYFACTORYWORKER_H_

// Using c++11 standard.
#include <chrono>
#include <thread>

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SystemLibraryProducts.h"
#include "core/wrapper/ConcurrencyFactoryWorker.h"

#include "core/wrapper/WIN32/WIN32Thread.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API ConcurrencyFactoryWorker<SystemLibraryWin32>
        {
            public:
                static Thread* createThread(Runnable &runnable)
                {
                    return new core::wrapper::WIN32Impl::WIN32Thread(runnable);
                };

				static void usleep(const long &microseconds) {
					std::this_thread::sleep_until(std::chrono::steady_clock::now() + std::chrono::microseconds(microseconds));
                };
        };
    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32_WIN32CONCURRENCYFACTORY_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTCONCURRENCYFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTCONCURRENCYFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SystemLibraryProducts.h"
#include "core/wrapper/ConcurrencyFactoryWorker.h"

#include "core/wrapper/Boost/BoostThread.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API ConcurrencyFactoryWorker<SystemLibraryBoost>
        {
            public:
                static Thread* createThread(Runnable &runnable)
                {
                    return new core::wrapper::Boost::BoostThread(runnable);
                }

                static void usleep(const long &microseconds)
                {
                    try {
                        ::boost::xtime xt;
                        ::boost::xtime_get(&xt, ::boost::TIME_UTC);

                        const long NANOSECONDS_PER_SECOND = 1000 * 1000 * 1000;
                        long nanoseconds = microseconds * 1000;
                        while (nanoseconds >= NANOSECONDS_PER_SECOND) {
                            nanoseconds -= NANOSECONDS_PER_SECOND;
                            xt.sec++;
                        }
                        // Add remaining nanoseconds.
                        xt.nsec += nanoseconds;

                        ::boost::thread::sleep(xt);

                    } catch (::boost::thread_interrupted &) {}
                }

        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTCONCURRENCYFACTORYWORKER_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTMUTEX_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTMUTEX_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/thread/mutex.hpp>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/MutexFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            class BoostCondition;

            /**
             * This class implements a mutex for protecting parts
             * using Boost.
             *
             * @See Mutex
             */
            class BoostMutex : public Mutex {
                    // The forbidden copy- and assignment operators are provided by boost.

                private:
                    friend class MutexFactoryWorker<SystemLibraryBoost>;
                    friend class BoostCondition;
                protected:
                    BoostMutex();

                public:
                    virtual ~BoostMutex();

                    virtual void lock();

                    virtual bool tryLock();

                    virtual void unlock();

                    boost::try_mutex& getNativeMutex();

                private:
                    boost::try_mutex m_mutex;
            };

        }
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTMUTEX_H_*/

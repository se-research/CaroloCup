/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTCONDITION_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTCONDITION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/thread/condition.hpp>

#include "core/wrapper/Condition.h"
#include "core/wrapper/ConditionFactoryWorker.h"

#include "core/wrapper/Boost/BoostMutex.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            /**
             * This class implements a condition for protecting parts
             * using Boost.
             *
             * @See Condition
             */
            class BoostCondition : public Condition {
                    // The forbidden copy- and assignment operators are provided by boost.

                private:
                    friend class ConditionFactoryWorker<SystemLibraryBoost>;

                    BoostCondition();

                public:
                    virtual ~BoostCondition();

                    virtual void waitOnSignal();

                    virtual bool waitOnSignalWithTimeout(const unsigned long ms);

                    virtual void wakeOne();

                    virtual void wakeAll();

                    virtual void lock();

                    virtual bool tryLock();

                    virtual void unlock();

                private:
                    boost::condition m_condition;
                    BoostMutex m_mutex;
            };

        }
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTCONDITION_H_*/

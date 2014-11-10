/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32CONDITION_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32CONDITION_H_

// Using c++11 standard.
#include <condition_variable>

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Condition.h"
#include "core/wrapper/ConditionFactoryWorker.h"
#include "core/wrapper/SystemLibraryProducts.h"
#include "core/wrapper/WIN32/WIN32Mutex.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            /**
             * This class implements a condition for protecting parts.
             *
             * @See Condition.
             */
            class WIN32Condition : public Condition {
                private:
                    friend class ConditionFactoryWorker<SystemLibraryWin32>;

                    WIN32Condition();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    WIN32Condition(const WIN32Condition &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    WIN32Condition& operator=(const WIN32Condition &);

                public:
                    virtual ~WIN32Condition();

                    virtual void waitOnSignal();

                    virtual bool waitOnSignalWithTimeout(const unsigned long ms);

                    virtual void wakeOne();

                    virtual void wakeAll();

                    virtual void lock();

                    virtual bool tryLock();

                    virtual void unlock();

                private:
                    std::condition_variable m_condition;
                    WIN32Mutex m_mutex;
            };

        }
    }
} // core::wrapper::WIN32Impl

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32CONDITION_H_*/

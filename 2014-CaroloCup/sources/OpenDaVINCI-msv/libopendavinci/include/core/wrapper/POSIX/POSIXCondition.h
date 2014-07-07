/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXCONDITION_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXCONDITION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Condition.h"
#include "core/wrapper/ConditionFactoryWorker.h"
#include "core/wrapper/SystemLibraryProducts.h"
#include "core/wrapper/POSIX/POSIXMutex.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class implements a condition for protecting parts
             * using pthread. It is derived from POSIXMutex for getting
             * the native mutex handle in pthread_cond_wait(...) calls.
             *
             * @See Condition.
             */
            class POSIXCondition : public Condition {
                private:
                    friend class ConditionFactoryWorker<SystemLibraryPosix>;

                    POSIXCondition();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXCondition(const POSIXCondition &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXCondition& operator=(const POSIXCondition &);

                public:
                    virtual ~POSIXCondition();

                    virtual void waitOnSignal();

                    virtual bool waitOnSignalWithTimeout(const unsigned long ms);

                    virtual void wakeOne();

                    virtual void wakeAll();

                    virtual void lock();

                    virtual bool tryLock();

                    virtual void unlock();

                private:
                    pthread_cond_t m_condition;
                    POSIXMutex m_mutex;
            };

        }
    }
} // core::wrapper::POSIX

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXCONDITION_H_*/

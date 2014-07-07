/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTIME_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTIME_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TimeFactoryWorker.h"
#include "core/wrapper/Time.h"


namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class implements the time using POSIX.
             *
             * @See Time
             */
            class POSIXTime : public Time {
                private:
                    friend class TimeFactoryWorker<SystemLibraryPosix>;

                    POSIXTime();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXTime(const POSIXTime &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXTime& operator=(const POSIXTime &);

                public:
                    virtual ~POSIXTime();

                    virtual int32_t getSeconds() const;

                    virtual int32_t getPartialMicroseconds() const;

                private:
                    int32_t m_seconds;
                    int32_t m_partialMicroseconds;
            };

        }
    }
} // core::wrapper::POSIX

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXTIME_H_*/

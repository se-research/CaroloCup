/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TIME_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TIME_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/TimeFactoryWorker.h"
#include "core/wrapper/Time.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            /**
             * This class implements the time using WIN32.
             *
             * @See Time
             */
            class WIN32Time : public Time {
                private:
                    friend class TimeFactoryWorker<SystemLibraryWin32>;

                    WIN32Time();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    WIN32Time(const WIN32Time &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    WIN32Time& operator=(const WIN32Time &);

                public:
                    virtual ~WIN32Time();

                    virtual int32_t getSeconds() const;

                    virtual int32_t getPartialMicroseconds() const;

                private:
                    int32_t m_seconds;
                    int32_t m_partialMicroseconds;
            };

        }
    }
} // core::wrapper::WIN32Impl

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32TIME_H_*/

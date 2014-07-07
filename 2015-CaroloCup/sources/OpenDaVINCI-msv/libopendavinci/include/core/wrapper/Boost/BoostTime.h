/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTIME_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTIME_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Time.h"
#include "core/wrapper/TimeFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            /**
             * This class implements the time using Boost.
             *
             * @See Time
             */
            class BoostTime : public Time {
                private:
                    friend class TimeFactoryWorker<SystemLibraryBoost>;

                    BoostTime();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BoostTime(const BoostTime &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BoostTime& operator=(const BoostTime &);

                public:
                    virtual ~BoostTime();

                    virtual int32_t getSeconds() const;

                    virtual int32_t getPartialMicroseconds() const;

                private:
                    int32_t m_seconds;
                    int32_t m_partialMicroseconds;
            };

        }
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTTIME_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_TIME_H_
#define OPENDAVINCI_CORE_WRAPPER_TIME_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        /**
         * This interface encapsulates all methods necessary for
         * getting the time since Jan. 1, 1970.
         *
         * @See TimeFactory
         */
        class Time {
            public:
                virtual ~Time();

                /**
                 * This method returns the seconds since
                 * Jan. 1, 1970.
                 *
                 * @return Seconds since Jan. 1, 1970.
                 */
                virtual int32_t getSeconds() const = 0;

                /**
                 * This method returns the partial microseconds from
                 * the next full second.
                 *
                 * @return Partial microseconds from the next full second.
                 */
                virtual int32_t getPartialMicroseconds() const = 0;

        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_TIME_H_*/

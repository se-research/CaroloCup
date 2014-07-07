/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_TIMECONSTANTS_H_
#define CONTEXT_BASE_TIMECONSTANTS_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace context {
    namespace base {

        /**
         * This class contains constants.
         */
        class OPENDAVINCI_API TimeConstants {
            public:
                enum EXECUTIONTIMES {
                    ONE_MILLISECOND = 1,
                    ONE_SECOND_IN_MILLISECONDS = 1000 * ONE_MILLISECOND,
                };

                enum CONTROLTIMES {
                    ONE_MILLISECOND_IN_MICROSECONDS = 1000,
                    ONE_SECOND_IN_MICROSECONDS = 1000 * ONE_MILLISECOND_IN_MICROSECONDS,
                    MAX_WAIT_FOR_REACHING_BREAKPOINT_PER_CYCLE = 15 * ONE_SECOND_IN_MICROSECONDS,
                };
        };

    }
} // context::base

#endif /*CONTEXT_BASE_TIMECONSTANTS_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_DATA_CONSTANTS_H_
#define OPENDAVINCI_CORE_DATA_CONSTANTS_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace data {

        /**
         * This class summarizes all constants as defined in Constants.cpp.
         */
        class OPENDAVINCI_API Constants {
            public:
                static const double PI;
                static const double DEG2RAD;
                static const double RAD2DEG;
                static const double MS2KMH;
                static const double KMH2MS;
                static const double G;
        };

    }
} // core::data

#endif /*OPENDAVINCI_CORE_DATA_CONSTANTS_H_*/

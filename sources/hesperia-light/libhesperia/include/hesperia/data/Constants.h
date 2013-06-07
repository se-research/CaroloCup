/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_CONSTANTS_H_
#define HESPERIA_CORE_DATA_CONSTANTS_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace data {

        /**
         * This class summarizes all constants as defined in Constants.cpp.
         */
        class HESPERIA_API Constants {
            public:
                static const double PI;
                static const double DEG2RAD;
                static const double RAD2DEG;
                static const double MS2KMH;
                static const double KMH2MS;
                static const double G;
        };

    }
} // hesperia::data

#endif /*HESPERIA_CORE_DATA_CONSTANTS_H_*/

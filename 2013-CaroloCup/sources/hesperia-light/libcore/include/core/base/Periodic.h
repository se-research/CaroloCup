/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_PERIODIC_H_
#define HESPERIA_CORE_BASE_PERIODIC_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This interface encapsulates all methods for realizing a periodic
         * execution.
         */
        class HESPERIA_API Periodic {
            public:
                virtual ~Periodic();

                /**
                 * This method returns the runtime frequency.
                 *
                 * @return Runtime frequency.
                 */
                virtual float getFrequency() const = 0;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_PERIODIC_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/data/Constants.h"

namespace hesperia {
    namespace data {

        const double Constants::PI = 3.141592654;
        const double Constants::DEG2RAD = (3.141592654 / 180.0);
        const double Constants::RAD2DEG = (1.0 / Constants::DEG2RAD);
        const double Constants::MS2KMH = 3.6;
        const double Constants::KMH2MS = (1.0 / Constants::MS2KMH);
        const double Constants::G = 9.81;

    }
} // core::data

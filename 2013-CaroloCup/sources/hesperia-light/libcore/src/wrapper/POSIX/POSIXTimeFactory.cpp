/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/POSIX/POSIXTime.h"
#include "core/wrapper/POSIX/POSIXTimeFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXTimeFactory::POSIXTimeFactory() {}

            POSIXTimeFactory::~POSIXTimeFactory() {}

            Time* POSIXTimeFactory::now() {
                return new POSIXTime();
            }

        }
    }
} // core::wrapper::POSIX

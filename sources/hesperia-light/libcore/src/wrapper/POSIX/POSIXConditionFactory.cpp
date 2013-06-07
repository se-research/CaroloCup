/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/POSIX/POSIXCondition.h"
#include "core/wrapper/POSIX/POSIXConditionFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXConditionFactory::POSIXConditionFactory() {}

            POSIXConditionFactory::~POSIXConditionFactory() {}

            Condition* POSIXConditionFactory::createCondition() {
                return new POSIXCondition();
            }

        }
    }
} // core::wrapper::POSIX

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONDITIONFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_CONDITIONFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Condition.h"

namespace core {
    namespace wrapper {

        /**
         * Factory for creating conditions using different
         * implementations (i.e. Boost or POSIX).
         *
         * It can be used as follows:
         *
         * @code
         * Condition *c = NULL;
         *
         * try {
         *     c = ConditionFactory::getInstance().createCondition();
         * }
         * catch(string &s) {
         *    clog << "Failed: " << s << endl;
         * }
         *
         * if (c != NULL) {
         *     c->waitOnSignal();
         * }
         *
         * ...
         * // Another thread:
         *
         * if (c != NULL) {
         *     c->wakeAll();
         * }
         *
         * ...
         * // Original thread:
         * if (c != NULL) {
         *     delete c;
         * }
         *
         * @endcode
         */
        struct OPENDAVINCI_API ConditionFactory
        {
            /**
             * This method returns the condition.
             */
            static Condition* createCondition();
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_CONDITIONFACTORY_H_*/

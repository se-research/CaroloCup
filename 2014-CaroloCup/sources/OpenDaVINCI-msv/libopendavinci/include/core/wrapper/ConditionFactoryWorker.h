/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONDITIONFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_CONDITIONFACTORYWORKER_H_

#include "core/wrapper/SystemLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * ConditionFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the ConditionFactoryWorker template class.
         *
         * @See ConditionFactory, ConditionFactoryWorker, SystemLibraryProducts,
         *      BoostConditionFactoryWorker, POSIXConditionFactoryWorker
         */
        template <SystemLibraryProducts product>
        class OPENDAVINCI_API ConditionFactoryWorker
        {
            public:
                /**
                 * This method returns the condition.
                 */
                static Condition* createCondition();
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_CONDITIONFACTORYWORKER_H_*/

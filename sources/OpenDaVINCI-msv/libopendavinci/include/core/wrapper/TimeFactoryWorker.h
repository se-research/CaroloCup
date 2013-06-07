/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_TIMEFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_TIMEFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Time.h"
#include "core/wrapper/SystemLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * TimeFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the TimeFactoryWorker template class.
         *
         * @See TimeFactory, TimeFactoryWorker,
         *      SystemLibraryProducts, BoostTimeFactoryWorker,
         *      POSIXTimeFactoryWorker
         *
         */

        template <SystemLibraryProducts product>
        class OPENDAVINCI_API TimeFactoryWorker
        {
            public:
                /**
                 * This method returns the wrapped time.
                 *
                 * @return time based on the type of instance this factory is.
                 */
                static Time* now();
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_TIMEFACTORYWORKER_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/ConditionFactory.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"
#include "core/wrapper/ConditionFactoryWorker.h"
#include "core/wrapper/SystemLibraryProducts.h"

#ifdef WIN32
    #include "core/wrapper/WIN32/WIN32ConditionFactoryWorker.h"
#endif
#ifndef WIN32
    #include "core/wrapper/POSIX/POSIXConditionFactoryWorker.h"
#endif

namespace core {
    namespace wrapper {

        Condition* ConditionFactory::createCondition()
        {
            typedef ConfigurationTraits<SystemLibraryProducts>::configuration configuration;

            return ConditionFactoryWorker<configuration::value>::createCondition();
        }
    }
} // core::wrapper

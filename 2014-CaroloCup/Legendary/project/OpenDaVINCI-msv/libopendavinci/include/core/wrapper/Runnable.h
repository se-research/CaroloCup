/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_RUNNABLE_H_
#define OPENDAVINCI_CORE_WRAPPER_RUNNABLE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        /**
         * This class provides a threadable interface. Any class that
         * wants to be able to start a new thread must implement
         * this interface.
         *
         * @See Thread
         */
        class OPENDAVINCI_API Runnable {
            public:
                virtual ~Runnable();

                /**
                 * This method returns true, iff this runnable is in its
                 * internal state for running.
                 *
                 * @return true iff this runnable is in its internal state for running.
                 */
                virtual bool isRunning() = 0;

                /**
                 * This method implements the body of the function
                 * to be "threadified".
                 */
                virtual void run() = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_RUNNABLE_H_*/

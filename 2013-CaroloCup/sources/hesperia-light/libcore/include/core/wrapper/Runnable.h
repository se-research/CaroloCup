/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_RUNNABLE_H_
#define HESPERIA_CORE_WRAPPER_RUNNABLE_H_

#include "core/native.h"

namespace core {
    namespace wrapper {

        /**
         * This class provides a threadable interface. Any class that
         * wants to be able to start a new thread must implement
         * this interface.
         *
         * @See Thread
         */
        class HESPERIA_API Runnable {
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

#endif /*HESPERIA_CORE_WRAPPER_RUNNABLE_H_*/

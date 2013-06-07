/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_STRINGOBSERVER_H_
#define HESPERIA_CORE_WRAPPER_STRINGOBSERVER_H_

#include "core/native.h"

#include "core/wrapper/StringListener.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for registering
         * as a string listener at a string observer.
         */
        class HESPERIA_API StringObserver {
            public:
                virtual ~StringObserver();

                /**
                 * This method sets or unsets a string listener.
                 *
                 * @param sl StringListener to be set. If set to NULL, observing is suspended.
                 */
                virtual void setStringListener(StringListener *sl) = 0;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_STRINGOBSERVER_H_*/

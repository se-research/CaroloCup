/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_STRINGLISTENER_H_
#define OPENDAVINCI_CORE_WRAPPER_STRINGLISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for getting informed
         * about new strings by a string observer.
         */
        class OPENDAVINCI_API StringListener {
            public:
                virtual ~StringListener();

                /**
                 * This method is called whenever a new string occurs.
                 *
                 * @param s String that has been occured.
                 */
                virtual void nextString(const string &s) = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_STRINGLISTENER_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_STRINGLISTENER_H_
#define HESPERIA_CORE_WRAPPER_STRINGLISTENER_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for getting informed
         * about new strings by a string observer.
         */
        class HESPERIA_API StringListener {
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

#endif /*HESPERIA_CORE_WRAPPER_STRINGLISTENER_H_*/

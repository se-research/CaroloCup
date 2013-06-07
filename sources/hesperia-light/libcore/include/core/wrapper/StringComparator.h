/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_STRINGCOMPARATOR_H_
#define HESPERIA_CORE_WRAPPER_STRINGCOMPARATOR_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class compares strings for sorting.
         */
        class HESPERIA_API StringComparator {
            public:
                StringComparator();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                StringComparator(const StringComparator &obj);

                virtual ~StringComparator();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                StringComparator& operator=(const StringComparator &obj);

                /**
                 * This method returns s1.compare(s2).
                 *
                 * @return s1.compare(s2) < 0.
                 */
                bool operator()(const string& s1, const string& s2) const;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_STRINGCOMPARATOR_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_STRINGCOMPARATOR_H_
#define OPENDAVINCI_CORE_WRAPPER_STRINGCOMPARATOR_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class compares strings for sorting.
         */
        class OPENDAVINCI_API StringComparator {
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

#endif /*OPENDAVINCI_CORE_WRAPPER_STRINGCOMPARATOR_H_*/

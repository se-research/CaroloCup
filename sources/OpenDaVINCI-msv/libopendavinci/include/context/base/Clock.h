/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_CLOCK_H_
#define CONTEXT_BASE_CLOCK_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "context/base/ControlledTime.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class provides a clock.
         */
        class OPENDAVINCI_API Clock {
            public:
                Clock();

                /**
                 * Copy constructor.
                 *
                 * @param obj Object from same type.
                 */
                Clock(const Clock &obj);

                virtual ~Clock();

                /**
                 * Assignment operator.
                 *
                 * @param obj Object from same type.
                 * @return (*this).
                 */
                Clock& operator=(const Clock &obj);

                /**
                 * This method returns the actual time.
                 *
                 * @return Actual time.
                 */
                const ControlledTime now() const;

                /**
                 * This method increments the clock for
                 * the given amount of milliseconds.
                 *
                 * @param ms.
                 */
                void increment(const uint32_t &ms);

            private:
                ControlledTime m_theTime;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_CLOCK_H_*/

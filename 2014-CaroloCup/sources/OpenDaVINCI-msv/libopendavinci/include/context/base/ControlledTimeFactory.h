/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_CONTROLLEDTIMEFACTORY_H_
#define CONTEXT_BASE_CONTROLLEDTIMEFACTORY_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Mutex.h"
#include "core/wrapper/TimeFactory.h"

#include "context/base/ControlledTime.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class provides controlled time factory.
         */
        class OPENDAVINCI_API ControlledTimeFactory : public core::wrapper::TimeFactory {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ControlledTimeFactory(const ControlledTimeFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ControlledTimeFactory& operator=(const ControlledTimeFactory &);

            public:
                ControlledTimeFactory();

                virtual ~ControlledTimeFactory();

                virtual core::wrapper::Time* now();

                /**
                 * This method sets the time.
                 *
                 * @param t Time.
                 */
                void setTime(const ControlledTime &ct);

            private:
                core::base::Mutex m_timeMutex;
                ControlledTime m_time;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_CONTROLLEDTIMEFACTORY_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_SYSTEMREPORTINGCOMPONENT_H_
#define CONTEXT_BASE_SYSTEMREPORTINGCOMPONENT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "context/base/SystemContextComponent.h"
#include "core/wrapper/Time.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This interface can be used to compute reporting data.
         */
        class OPENDAVINCI_API SystemReportingComponent : public SystemContextComponent {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SystemReportingComponent(const SystemReportingComponent&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SystemReportingComponent& operator=(const SystemReportingComponent&);

            protected:
                /**
                 * Protected constructor to enforce subclasses.
                 */
                SystemReportingComponent();

            public:
                virtual ~SystemReportingComponent();

                /**
                 * This method is called whenever any application component
                 * was activated to validate the new situation.
                 *
                 * @param t Current system time.
                 */
                virtual void report(const core::wrapper::Time &t) = 0;

            private:
                /**
                 * This method returns the fixed frequency of 0 since
                 * reporting components are unscheduled but called
                 * right after any application was executed.
                 */
                virtual float getFrequency() const;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_SYSTEMREPORTINGCOMPONENT_H_*/

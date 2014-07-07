/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_RUNNER_H_
#define CONTEXT_BASE_RUNNER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Periodic.h"
#include "core/wrapper/Time.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class is the base class for periodic executions.
         */
        class OPENDAVINCI_API Runner : public core::base::Periodic {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                Runner(const Runner&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                Runner& operator=(const Runner&);

            protected:
                /**
                 * Protected constructor to enforce subclasses.
                 */
                Runner();

            public:
                virtual ~Runner();

                /**
                 * This method returns true if the app  needs to be executed.
                 *
                 * @param t Time.
                 * @return True if the app needs to be executed at time t.
                 */
                bool needsExecution(const core::wrapper::Time &t) const;

                /**
                 * This method should be overridden in subclasses to add an additional
                 * condition to the time needsExecution indicating whether an
                 * application has finished. This implementation simply
                 * returns always true.
                 *
                 * @return true.
                 */
                virtual bool hasFinished() const;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_RUNNER_H_*/

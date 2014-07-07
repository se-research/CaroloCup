/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "context/base/Runner.h"
#include "context/base/TimeConstants.h"

namespace context {
    namespace base {

        using namespace std;

        Runner::Runner() {}

        Runner::~Runner() {}

        bool Runner::hasFinished() const {
            return false;
        }

        bool Runner::needsExecution(const core::wrapper::Time &t) const {
            bool retVal = false;
            if (getFrequency() > 0) {
                const uint32_t THIS_RUN_AT_TIME = static_cast<uint32_t>(TimeConstants::ONE_SECOND_IN_MILLISECONDS / getFrequency());

                // Consider the current seconds only if the module is running with a frequency slower than 1 Hz.
                const uint32_t CURRENT_MILLISECONDS = ( (THIS_RUN_AT_TIME > TimeConstants::ONE_SECOND_IN_MILLISECONDS) ? t.getSeconds() * TimeConstants::ONE_SECOND_IN_MILLISECONDS : 0 )
                                                      + t.getPartialMicroseconds() / TimeConstants::ONE_MILLISECOND_IN_MICROSECONDS;

                retVal = ( (CURRENT_MILLISECONDS % THIS_RUN_AT_TIME) == 0 );
            }

            return (retVal && !hasFinished());
        }

    }
} // context::base

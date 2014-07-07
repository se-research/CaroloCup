/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef RECORDERMODULE_H_
#define RECORDERMODULE_H_

#include "core/base/ConferenceClientModule.h"

namespace recorder {

    using namespace std;

    /**
     * This class can be used to record data distributed
     * using a conference.
     */
    class RecorderModule : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            RecorderModule(const RecorderModule &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            RecorderModule& operator=(const RecorderModule &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            RecorderModule(const int32_t &argc, char **argv);

            virtual ~RecorderModule();

            virtual void wait();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();
    };

} // recorder

#endif /*RECORDERMODULE_H_*/

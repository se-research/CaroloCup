/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLAYERMODULE_H_
#define PLAYERMODULE_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"

namespace player {

    using namespace std;

    /**
     * This class can be used to replay previously recorded
     * data using a conference for distribution.
     */
    class PlayerModule : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            PlayerModule(const PlayerModule &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            PlayerModule& operator=(const PlayerModule &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            PlayerModule(const int32_t &argc, char **argv);

            virtual ~PlayerModule();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void wait();

        private:
            core::base::FIFOQueue m_playerControl;

            virtual void setUp();

            virtual void tearDown();
    };

} // player

#endif /*PLAYERMODULE_H_*/

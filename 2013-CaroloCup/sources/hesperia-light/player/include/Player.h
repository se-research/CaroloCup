/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLAYER_H_
#define PLAYER_H_

#include "hesperia/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"

namespace player {

    using namespace std;

    /**
     * This class can be used to replay previously recorder
     * data using a conference for distribution.
     */
    class Player : public hesperia::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Player(const Player &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Player& operator=(const Player &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Player(const int32_t &argc, char **argv);

            virtual ~Player();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void wait();

        private:
            core::base::FIFOQueue m_playerControl;

            virtual void setUp();

            virtual void tearDown();
    };

} // player

#endif /*PLAYER_H_*/

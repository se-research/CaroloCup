/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef REC2STDOUT_H_
#define REC2STDOUT_H_

#include "core/base/ConferenceClientModule.h"

namespace rec2stdout {

    using namespace std;

    /**
     * This class converts a given previously recorded data file
     * to a video.
     */
    class Rec2Stdout : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Rec2Stdout(const Rec2Stdout &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Rec2Stdout& operator=(const Rec2Stdout &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Rec2Stdout(const int32_t &argc, char **argv);

            virtual ~Rec2Stdout();

            virtual void setUp();

            virtual void tearDown();

            core::base::ModuleState::MODULE_EXITCODE body();
    };

} // rec2stdout

#endif /*REC2STDOUT_H_*/

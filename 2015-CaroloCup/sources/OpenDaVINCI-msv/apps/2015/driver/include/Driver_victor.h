/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DRIVER_VICTOR_H_
#define DRIVER_VICTOR_H_

#include "core/base/ConferenceClientModule.h"

namespace msv {

    using namespace std;

    /**
     * This class is a skeleton to send driving commands to Hesperia-light's vehicle driving dynamics simulation.
     */
    class Driver_victor : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Driver_victor(const Driver_victor &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Driver_victor& operator=(const Driver_victor &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Driver_victor(const int32_t &argc, char **argv);


            virtual ~Driver_victor();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();
	    
	    int carstate;			// What the car is doing at the moment. 3 = parking
	    int interval;			// Used for timer to define how long time to do something
	    int parkingState;			// Used for different steps during parking
	    double speed;			// Speed of the car
	    double steering;			// Angle of the wheels
	    
            virtual void tearDown();
    };

} // msv

#endif /*DRIVER_H_*/

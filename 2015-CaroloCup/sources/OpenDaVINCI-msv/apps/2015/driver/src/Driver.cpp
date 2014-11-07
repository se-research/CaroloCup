/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <stdio.h>
#include <math.h>

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"
#include "core/data/Constants.h"
#include "core/data/control/VehicleControl.h"
#include "core/data/environment/VehicleData.h"

// Data structures from msv-data library:
#include "SteeringData.h"
#include "SensorBoardData.h"
#include "UserButtonData.h"

#include "Driver.h"




	    timeval timer;				// General starttimer
	    timeval timer2;				// General timer that gets current time



namespace msv {

        using namespace std;
        using namespace core::base;
        using namespace core::data;
        using namespace core::data::control;
        using namespace core::data::environment;
	

        Driver::Driver(const int32_t &argc, char **argv) :
	        ConferenceClientModule(argc, argv, "Driver"),
	        carstate(3),
	        interval(0),
	        parkingState(-1),
	        speed(0.0),
	        steering(0.0)
		{
        }

        Driver::~Driver() {}

        void Driver::setUp() {
	        // This method will be call automatically _before_ running body().
        }

        void Driver::tearDown() {
	        // This method will be call automatically _after_ return from body().
        }

        // This method will do the main data processing job.
        ModuleState::MODULE_EXITCODE Driver::body() {

	        while (getModuleState() == ModuleState::RUNNING) {
                // In the following, you find example for the various data sources that are available:

		        // 1. Get most recent vehicle data:
		        Container containerVehicleData = getKeyValueDataStore().get(Container::VEHICLEDATA);
		        VehicleData vd = containerVehicleData.getData<VehicleData> ();
		        cerr << "Most recent vehicle data: '" << vd.toString() << "'" << endl;

		        // 2. Get most recent sensor board data:
		        Container containerSensorBoardData = getKeyValueDataStore().get(Container::USER_DATA_0);
		        SensorBoardData sbd = containerSensorBoardData.getData<SensorBoardData> ();
		        cerr << "Most recent sensor board data: '" << sbd.toString() << "'" << endl;

		        // 3. Get most recent user button data:
		        Container containerUserButtonData = getKeyValueDataStore().get(Container::USER_BUTTON);
		        UserButtonData ubd = containerUserButtonData.getData<UserButtonData> ();
		        cerr << "Most recent user button data: '" << ubd.toString() << "'" << endl;

		        // 4. Get most recent steering data as fill from lanedetector for example:
		        Container containerSteeringData = getKeyValueDataStore().get(Container::USER_DATA_1);
		        SteeringData sd = containerSteeringData.getData<SteeringData> ();
		        cerr << "Most recent steering data: '" << sd.toString() << "'" << endl;



                // Design your control algorithm here depending on the input data from above.

	if (carstate == 3) {
	  
		cerr << "parkingState: '" << parkingState << "'" << endl;
	  
		speed = 1.0;
		
		// Begin parking sequence once an object is detected
		if (sbd.getDistance(2) > 0.0 && parkingState == -1) {
			parkingState = 0;
		}
			
	      
		// start the timer when an empty space is detected
		if (sbd.getDistance(2) < 0.0 && parkingState == 0) { 
			parkingState = 1;
			gettimeofday(&timer, 0);
		}
		
		// start timer2 when object is detected
		if (sbd.getDistance(2) > 0.0 && parkingState == 1) { 
			parkingState = 2;
			gettimeofday(&timer2, 0);
			interval = 5500;
		}
		
		// checking if the space is large enough for parking
		
		if (((timer2.tv_sec - timer.tv_sec) * 1000) + ((timer2.tv_usec - timer.tv_usec) / 1000) * speed > interval && parkingState == 2) { 
			parkingState = 3;
			interval = 12500;
			gettimeofday(&timer, 0);
		}
		else if (((timer2.tv_sec - timer.tv_sec) * 1000) + ((timer2.tv_usec - timer.tv_usec) / 1000) * speed <= interval && parkingState == 2) {
			parkingState = 0;
		}
		
		// Turning right backwords to park
		if (parkingState == 3) {
		        gettimeofday(&timer2, 0);
			speed = -0.5;
			steering = 25 * Constants::DEG2RAD;
			if (((timer2.tv_sec - timer.tv_sec) * 1000) + ((timer2.tv_usec - timer.tv_usec) / 1000) <= 900) {
			  speed = 1.0;
			}

			if (((timer2.tv_sec - timer.tv_sec) * 1000) + ((timer2.tv_usec - timer.tv_usec) / 1000) <= 2000) {
			  steering = 0 * Constants::DEG2RAD;
			}

			if (((timer2.tv_sec - timer.tv_sec) * 1000) + ((timer2.tv_usec - timer.tv_usec) / 1000) >= interval) { 
			    interval = 12000 ;
			    gettimeofday(&timer, 0);
			    parkingState = 4;
			}
			cerr << "interval3: '" << (((timer2.tv_sec - timer.tv_sec) * 1000) + ((timer2.tv_usec - timer.tv_usec) / 1000)) << "'" << endl;
		}
		
		

		
		// Straightening the car
		if (parkingState == 4) {
		        gettimeofday(&timer2, 0);
			speed = -0.5;
			steering = -25 * Constants::DEG2RAD;
			if (((timer2.tv_sec - timer.tv_sec) * 1000) + ((timer2.tv_usec - timer.tv_usec) / 1000) * speed >= interval || (sbd.getDistance(0) < 0.5 && sbd.getDistance(1) > 0.0) ) { 
			    speed = 0.0;
			    parkingState = 5;
			    
			}
		}
		if (parkingState == 5) {
			speed = 0.5;
			steering = 25 * Constants::DEG2RAD;
			if (abs(sbd.getDistance(0) - sbd.getDistance(1)) < 0.1) {
			    speed = 0.0;
			    parkingState = 6;
			}
		}
		if (parkingState == 6) {
			speed = -0.5;
			steering = 0;
			if (abs(sbd.getDistance(0)) < 1.0) {
			    speed = 0.0;
			}
		}
		



		        // Create vehicle control data.
		        VehicleControl vc;

                // With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
		        vc.setSpeed(speed);

                // With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
//                 double desiredSteeringWheelAngle = 0; // 4 degree but SteeringWheelAngle expects the angle in radians!
		        vc.setSteeringWheelAngle(steering);

                // You can also turn on or off various lights:
                vc.setBrakeLights(false);
                vc.setLeftFlashingLights(false);
                vc.setRightFlashingLights(true);

		        // Create container for finally sending the data.
		        Container c(Container::VEHICLECONTROL, vc);
		        // Send container.
		        getConference().send(c);
	        }

	       
        }
        
	return ModuleState::OKAY;
	}
} // msv


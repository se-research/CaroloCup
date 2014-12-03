/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 *
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

namespace msv {

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;
using namespace core::data::environment;

int MinParkingDist = 550;
int Distance;
int CurrentDist;
int DesiredDistance1 = 150; //700 is required 550+150;
int DesiredDistance2 = 360;
int DesiredDistance3 = 360;
int DesiredDistance4 = 100;
int DesiredDistance5 = 130;
double IRdis_SL;
double IRdis_RL;
double IRdis_RR;
double IRdis_SR;


Driver::Driver(const int32_t &argc, char **argv) :
		ConferenceClientModule(argc, argv, "Driver"), driving_state(DRIVE), parking_state(
				BACKWARDS_RIGHT) {
}

Driver::~Driver() {
}

void Driver::setUp() {
	// This method will be call automatically _before_ running body().
}

void Driver::tearDown() {
	// This method will be call automatically _after_ return from body().
}


double start_timer;
double time_taken;

double driving_speed;			// Speed of the car
double desiredSteeringWheelAngle;// Angle of the wheels

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Driver::body() {

	driving_state = DRIVE;
	parking_state = BACKWARDS_RIGHT;
	// Get configuration data.
	KeyValueConfiguration kv = getKeyValueConfiguration();
	//const uint32_t m_sensorId = kv.getValue<int32_t> ("irus.sensor2.id");
	//cout << "***********  Sensor ID: " << m_sensorId << endl;

	VehicleControl vc;

	TimeStamp start;


	while (getModuleState() == ModuleState::RUNNING) {
		// In the following, you find example for the various data sources that are available:

		// 1. Get most recent vehicle data:
		Container containerVehicleData = getKeyValueDataStore().get(
				Container::VEHICLEDATA);
		VehicleData vd = containerVehicleData.getData<VehicleData>();
		cerr << "Most recent vehicle data: '" << vd.toString() << "'" << endl;

		// 2. Get most recent sensor board data:
		Container containerSensorBoardData = getKeyValueDataStore().get(
				Container::USER_DATA_0);
		SensorBoardData sbd =
				containerSensorBoardData.getData<SensorBoardData>();
		cerr << "Most recent sensor board data: '" << sbd.toString() << "'"
				<< endl;

		// 3. Get most recent user button data:
		Container containerUserButtonData = getKeyValueDataStore().get(
				Container::USER_BUTTON);
		UserButtonData ubd = containerUserButtonData.getData<UserButtonData>();
		cerr << "Most recent user button data: '" << ubd.toString() << "'"
				<< endl;

		// 4. Get most recent steering data as fill from lanedetector for example:
		Container containerSteeringData = getKeyValueDataStore().get(
				Container::USER_DATA_1);
		SteeringData sd = containerSteeringData.getData<SteeringData>();
		cerr << "Most recent steering data: '" << sd.toString() << "'" << endl;

		//IRs
		IRdis_SL = sbd.getDistance(0); // Side Left IR
		IRdis_RL = sbd.getDistance(1); // Rear Left IR
		IRdis_RR = sbd.getDistance(2); // Rear Right IR
		IRdis_SR = sbd.getDistance(3); // Side Right IR
		
		//WheelEncoder
		Distance = sbd.getDistance(6); // WheeelEncoder Data (mm)
		
		//Status
		cout << " ===== Side Left Infrared reading: " << IRdis_SL << endl;
		cout << " ===== Rear Left Infrared reading: " << IRdis_RL << endl;
		cout << " ===== Rear Right Infrared reading: " << IRdis_RR << endl;
		cout << " ===== Side Right Infrared reading: " << IRdis_SR << endl;
		cout << " ===== WheeelEncoder Data (mm): " << Distance << endl;
		// Design your control algorithm here depending on the input data from above.
		switch (driving_state) {
		case DRIVE: {
			cout << "In drive mode" << endl;
			driving_speed = 1.0;
			desiredSteeringWheelAngle = 0;

			if (IRdis_SR < 10)
				driving_state = START_OBST;
		}
			break;

		case START_OBST: {
			cout << "Scanning Obstacle" << endl;
			driving_speed = 1.0;
			desiredSteeringWheelAngle = 0;
			if (IRdis_SR > 15) {
				driving_state = POSSIBLE_SPOT;
	
// 				TimeStamp currentTime_strt1;
// 				start = currentTime_strt1;
			}
		}
			break;

		case POSSIBLE_SPOT: {


			cout << "---- DIstance so far: " << Distance << endl;
			driving_speed = 1.0;
			desiredSteeringWheelAngle = 0;
			
			CurrentDist = Distance;
			if (IRdis_SR > 15 && Distance == (CurrentDist + MinParkingDist)) {
				
				desiredSteeringWheelAngle = 0;
				driving_state = STOP_FOR_PARKING;

				cout << "Found a parking spot" << endl;
			} else {

				driving_state = START_OBST;
			}
			
			cout << "Found a possible spot" << endl;
		}
			break;
		case Initialize_Pos_For_Parking: {
		  
			TimeStamp currentTime;
			CurrentDist = Distance;
			if (Distance == (CurrentDist + DesiredDistance1)) {
			  
				driving_speed = 0;
				driving_state = STOP_FOR_PARKING;
				start_timer = currentTime.toMicroseconds() / 1000000.0;
			}
		}
			break;

		case STOP_FOR_PARKING: {
		  
			TimeStamp currentTime2;
			time_taken = (currentTime2.toMicroseconds() / 1000000.0)
					- start_timer;
					
			cout << "++++++++++ Stoping timer: " << time_taken << endl;
			CurrentDist = Distance;
			if (time_taken > 4) {
				
				//parking(vc, vd);
				driving_state = PARKING;
				CurrentDist = Distance;

			}
		}
			break;

		case PARKING: {
			parking();
		}
			break;
		default: {

			cout << "Non of these states" << endl;

			driving_speed = 4;
			desiredSteeringWheelAngle = 0;

		}
		}

		// Create vehicle control data.

		// With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
		vc.setSpeed(driving_speed);

		// With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
		//double desiredSteeringWheelAngle = 0; // 4 degree but SteeringWheelAngle expects the angle in radians!
		vc.setSteeringWheelAngle(
				desiredSteeringWheelAngle * Constants::DEG2RAD);

		// You can also turn on or off various lights:
		vc.setBrakeLights(false);
		vc.setLeftFlashingLights(false);
		vc.setRightFlashingLights(true);

		// Create container for finally sending the data.
		Container c(Container::VEHICLECONTROL, vc);
		// Send container.
		getConference().send(c);
	}

	return ModuleState::OKAY;
}


void Driver::parking() {

	switch (parking_state) {
	case BACKWARDS_RIGHT: {

		CurrentDist = Distance;
		if (Distance == (CurrentDist + DesiredDistance2)) {
			parking_state = BACKWARDS_LEFT;
			driving_speed = -1;
			desiredSteeringWheelAngle = 25;

		} 
	}
		break;
		
	case BACKWARDS_LEFT: {

		CurrentDist = Distance;
		if (Distance == (CurrentDist + DesiredDistance3)) {

			driving_speed = -1;
			desiredSteeringWheelAngle = -25;
			parking_state = FORWARD_RIGHT;

		} 
	}
		break;

	case FORWARD_RIGHT: {

		CurrentDist = Distance;
		if (Distance == (CurrentDist + DesiredDistance4)) {

			driving_speed = 1;
			desiredSteeringWheelAngle = 25;
			parking_state = BACK_AGAIN;

		} 
	}

		break;

	case BACK_AGAIN: {

		CurrentDist = Distance;
		if (Distance == (CurrentDist + DesiredDistance5)) {

			driving_speed = -1;
			desiredSteeringWheelAngle = 0;
			parking_state = STOP;
		}

	}

		break;

	case STOP:
		driving_speed = 0;
		desiredSteeringWheelAngle = 0;

		cout << "****  stop the car  ****" << endl;

		break;
	default: {

		cout << "Non of these states" << endl;

		//driving_speed = 4;
		//desiredSteeringWheelAngle = 0;

	}
	}
}

} // msv

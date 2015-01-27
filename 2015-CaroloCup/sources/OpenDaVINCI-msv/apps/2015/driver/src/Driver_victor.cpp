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

#include "Driver_victor.h"

namespace msv {

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;
using namespace core::data::environment;

// int MinParkingDist = 500;
// int MaxParkingDist = 650;
//int SafeDistance = 30;
int Distance;
int CurrentDistSpot;
int CurrentDistSpot2;
int CurrentDist;
// int DesiredDistance1 = 100; //700 is required 550+150;
// int DesiredDistance2 = 530;
// int DesiredDistance3 = 490;
// int DesiredDistance4 = 90;
// int DesiredDistance5 = 30;
// int SpeedF1 = 5;
// int SpeedF2 = 5;
// int SpeedB1 = -4;
// int SpeedB2 = -7;
int CurrentDist1;
int CurrentDist2;
int CurrentDist3;
int CurrentDist4;
// int CurrentDist5;
int USFront;
int USRear;
int IRdis_SL;
int IRdis_RL;
int IRdis_RR;
int IRdis_SR;
int gapWidth = 0;


Driver::Driver(const int32_t &argc, char **argv) :
		ConferenceClientModule(argc, argv, "Driver"), driving_state(FORWARD){
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
double start_timer2;
double time_taken;
double time_taken2;
double start_timerB;
double time_takenB;


int driving_speed;			// Speed of the car
int desiredSteeringWheelAngle;// Angle of the wheels

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Driver::body() {
	driving_state = DRIVE;
	// Get configuration data.
	KeyValueConfiguration kv = getKeyValueConfiguration();
	//const uint32_t m_sensorId = kv.getValue<int32_t> ("irus.sensor2.id");
	//cout << "***********  Sensor ID: " << m_sensorId << endl;

	VehicleControl vc;

	TimeStamp start;
	//bool measuring = false;
	

	while (getModuleState() == ModuleState::RUNNING) {
		// In the following, you find example for the various data sources that are available:
    
		// 1. Get most recent vehicle data:
		Container containerVehicleData = getKeyValueDataStore().get(
				Container::VEHICLEDATA);
		VehicleData vd = containerVehicleData.getData<VehicleData>();
		// cerr << "Most recent vehicle data: '" << vd.toString() << "'" << endl;

		// 2. Get most recent sensor board data:
		Container containerSensorBoardData = getKeyValueDataStore().get(
				Container::USER_DATA_0);
		SensorBoardData sbd =
				containerSensorBoardData.getData<SensorBoardData>();
// 		cout << "Most recent sensor board data: '" << sbd.toString() << "'"
// 				<< endl;

		// 3. Get most recent user button data:
		Container containerUserButtonData = getKeyValueDataStore().get(
				Container::USER_BUTTON);
		UserButtonData ubd = containerUserButtonData.getData<UserButtonData>();
		// cerr << "Most recent user button data: '" << ubd.toString() << "'"
				// << endl;

		// 4. Get most recent steering data as fill from lanedetector for example:
		Container containerSteeringData = getKeyValueDataStore().get(
				Container::USER_DATA_1);
		SteeringData sd = containerSteeringData.getData<SteeringData>();
		// cerr << "Most recent steering data: '" << sd.toString() << "'" << endl;

		//IRs
		IRdis_SL = sbd.getDistance(0); // Side Left IR
		IRdis_RL = sbd.getDistance(1); // Rear Left IR
		IRdis_RR = sbd.getDistance(2); // Rear Right IR
		IRdis_SR = sbd.getDistance(3); // Side Right IR
		USFront = sbd.getDistance(4); // Front UltraSonic
		USRear = sbd.getDistance(5); // Rear UltraSonic 
		
		//WheelEncoder
		Distance = sbd.getDistance(6); // WheeelEncoder Data (mm)
		
		//Status
		cout << " ===== Rear IRs difference: " << abs (IRdis_RL - IRdis_RR) << endl;
// 		cout << " ===== Side Left Infrared reading: " << IRdis_SL << endl;
 		cout << " ===== Rear Left Infrared reading: " << IRdis_RL << endl;
 		cout << " ===== Rear Right Infrared reading: " << IRdis_RR << endl;
		cout << " ===== Side Right Infrared reading: " << IRdis_SR << endl;
		cout << " ===== Front UltraSonic reading: " << USFront << endl;
		cout << " ===== Rear UltraSonic reading: " << USRear << endl;
		cout << " ===== WheeelEncoder Data (mm): " << Distance << endl;
		cout << " ===== Driving_Speed: " << driving_speed << endl;
		cout << " ===== DesiredSteeringWheelAngle: " << desiredSteeringWheelAngle << endl;
		cout << " ===== Parking spot length: "<< gapWidth << endl;
// 		cout << "CurrentDistSpot "<< CurrentDistSpot << endl;
// 		cout << "CurrentDistSpot2 "<< CurrentDistSpot2 << endl;
// 		cout << "CurrentDist1 "<< CurrentDist1 << endl;
// 		cout << "CurrentDist2 "<< CurrentDist2 << endl;
// 		cout << "CurrentDist3 "<< CurrentDist3 << endl;
// 		cout << "CurrentDist4 "<< CurrentDist4 << endl;
// 		cout << "CurrentDist5 "<< CurrentDist5 << endl;
// 		
		// Design your control algorithm here depending on the input data from above.
		switch (driving_state) {
		case FORWARD: {
			cout << "        =========== FORWARD" << endl;
			driving_speed = 1;
			desiredSteeringWheelAngle = 0;
			
			if ((USFront < 30 && USFront > 2)){
				CurrentDist1 = Distance;
				driving_state = WAIT1;
				TimeStamp currentTime;
				start_timer = currentTime.toMicroseconds() / 1000.0;
			}
// 			if ((IRdis_SR < 23 && IRdis_SR > 2)){ 
// 				driving_state = START_OBST;
// 			
// 			}
		}
			break;

		case WAIT1: {
			cout << "        =========== WAIT1" << endl;
			driving_speed = 0;
			desiredSteeringWheelAngle = 0;
			
			TimeStamp currentTime2;
			time_taken = (currentTime2.toMicroseconds() / 1000.0) - start_timer;
			
			if (time_taken > 300) {  
 				
				//parking(vc, vd);
				CurrentDist2 = Distance;
				driving_state = BACKWARDS;
			}
		}
			break;

		case BACKWARDS: {
			driving_speed = -1;
			cout << "        =========== BACKWARDS" << endl;;
			desiredSteeringWheelAngle = 0;
			
			if ((USRear < 30 && USRear > 2)){ 
				driving_state = BACKWARDS;
				TimeStamp currentTime3;
				start_timer = currentTime3.toMicroseconds() / 1000.0;
			}

			if(IRdis_SR < 23 && IRdis_SR > 2){
			  gapWidth = Distance - CurrentDistSpot;
			  //CurrentDistSpot2 = Distance;
			 if((gapWidth > MinParkingDist) || (gapWidth > MaxParkingDist)){
			   desiredSteeringWheelAngle=0;
			   driving_state = INITIALIZE_POS_FOR_PARKING; 
			 }else{
			  driving_state = DRIVE; 
			 }
			}
			
			CurrentDist = Distance;
			cout << "\t Parking spot length: "<< gapWidth << endl;
		}
			break;
			
		case WAIT2: { 
		 
			cout << "        =========== WAIT2" << endl;
			driving_speed = 0;
			desiredSteeringWheelAngle = 0;
			
			TimeStamp currentTime4;
			time_taken2 = (currentTime4.toMicroseconds() / 1000.0) - start_timer2;
			
			if (time_taken2 > 300) {  
 				
				//parking(vc, vd);
				CurrentDist2 = Distance;
				driving_state = STOP; 
			}
		}
			break;
			
		case STOP: {
			cout << "        =========== STOP" << endl;
			driving_speed = 0;
			desiredSteeringWheelAngle = 0;
			
		}
			break;
	
		default: {

			cout << "Non of these states" << endl;

			driving_speed = 0;
			desiredSteeringWheelAngle = 0;

		}
		}

		// Create vehicle control data.

		// With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
		vc.setSpeed(driving_speed);

		// With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
		//double desiredSteeringWheelAngle = 0; // 4 degree but SteeringWheelAngle expects the angle in radians!
		vc.setSteeringWheelAngle(desiredSteeringWheelAngle);

		// You can also turn on or off various lights:
		vc.setBrakeLights(false);
		vc.setLeftFlashingLights(false);
		vc.setRightFlashingLights(true);

		// Create container for finally sending the data.
		Container c(Container::VEHICLECONTROL, vc);
		// Send container.
		getConference().send(c);
		
	}
	driving_speed = 0;
	return ModuleState::OKAY;
	//driving_speed = 0;
	}

} // msv
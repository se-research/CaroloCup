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

int MinParkingDist;
int MaxParkingDist;
int SafeDistance;
int MinSafeDistance;
int MoreStates;
int DesiredMoreStates;
int DesiredDistance1; 
int DesiredDistance2;
int DesiredDistance3;
int DesiredDistance4;
int DesiredDistance5;
int SpeedF1;
int SpeedF2;
int SpeedB1;
int SpeedB2;
int Stop_Speed;
int IRMaxDist;
int IRMinDist;
int Distance;
int CurrentDistSpot;
int CurrentDistSpot2;
int CurrentDist;
int CurrentDist1;
int CurrentDist2;
int CurrentDist3;
int CurrentDist4;
int CurrentDist5;
int USFront;
int USRear;
int IRdis_SL;
int IRdis_RL;
int IRdis_RR;
int IRdis_SR;
bool rightIndicator = false;
bool leftIndicator = false;
bool brakeIndicator = false;
bool initialized = false;
bool isSmallGapSize;
double parkAngle;


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
double start_timer2;
double time_taken;
double time_taken2;
double start_timerB;
double time_takenB;
double start_timerIndicator;
double time_takenIndicator;



int driving_speed;	// Speed of the car
int desiredSteeringWheelAngle;// Angle of the wheels

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Driver::body() {
	driving_state = DRIVE;
	parking_state = BACKWARDS_RIGHT;
	// Get configuration data.
	if(!initialized) {
		KeyValueConfiguration kv = getKeyValueConfiguration();
		MinParkingDist = kv.getValue<uint32_t> ("driver.MinParkingDist");
		MaxParkingDist = kv.getValue<uint32_t> ("driver.MaxParkingDist");
		SafeDistance = kv.getValue<uint32_t> ("driver.SafeDistance");
		DesiredDistance1 = kv.getValue<uint32_t> ("driver.DesiredDistance1");
		DesiredDistance2 = kv.getValue<uint32_t> ("driver.DesiredDistance2");
		DesiredDistance3 = kv.getValue<uint32_t> ("driver.DesiredDistance3");
		DesiredDistance4 = kv.getValue<uint32_t> ("driver.DesiredDistance4");
		DesiredDistance5 = kv.getValue<uint32_t> ("driver.DesiredDistance5");
		SpeedF1 = kv.getValue<uint32_t> ("driver.SpeedF1");
		SpeedF2 = kv.getValue<uint32_t> ("driver.SpeedF2");
		SpeedB1 = kv.getValue<uint32_t> ("driver.SpeedB1");
		SpeedB2 = kv.getValue<uint32_t> ("driver.SpeedB2");
		isSmallGapSize = kv.getValue<uint32_t> ("driver.isSmallGapSize");
		IRMaxDist = kv.getValue<uint32_t> ("driver.IRMaxDist");
		IRMinDist = kv.getValue<uint32_t> ("driver.IRMinDist");
		MinSafeDistance = kv.getValue<uint32_t> ("driver.MinSafeDistance");
		DesiredMoreStates = kv.getValue<uint32_t> ("driver.DesiredMoreStates");
		MoreStates = kv.getValue<uint32_t> ("driver.MoreStates");
		//cout << "***********  Sensor ID: " << m_sensorId << endl;	
		initialized = true;

		cout << "initilized!" << endl;
	}
	
	cout << "MinParkingDist" << MinParkingDist << endl;
	cout << "MaxParkingDist" << MaxParkingDist << endl;
	cout << "SafeDistance" << SafeDistance << endl;
	cout << "DesiredDistance1" << DesiredDistance1 << endl;
	cout << "DesiredDistance2" << DesiredDistance2 << endl;
	cout << "DesiredDistance3" << DesiredDistance3 << endl;
	cout << "DesiredDistance4" << DesiredDistance4 << endl;
	cout << "DesiredDistance5" << DesiredDistance5 << endl;
	cout << "SpeedF1" << SpeedF1 << endl;
	cout << "SpeedF2" << SpeedF2 << endl;
	cout << "SpeedB1" << SpeedB1 << endl;
	cout << "SpeedB2" << SpeedB2 << endl;
	cout << "isSmallGapSize" << isSmallGapSize << endl;
	cout << "IRMaxDist" << IRMaxDist << endl;
	cout << "IRMinDist" << IRMinDist << endl;
	cout << "MinSafeDistance" << MinSafeDistance << endl;
	cout << "DesiredMoreStates" << DesiredMoreStates << endl;
	cout << "MoreStates" << MoreStates << endl;

	VehicleControl vc;

	TimeStamp start;
	int gapWidth = 0;
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
 		cout << "Most recent sensor board data: '" << sbd.toString() << "'"
 				<< endl;

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

		//Sensors
		IRdis_SL = sbd.getDistance(3); // Side Left IR // *on legendary is the Front-Side-Right
		IRdis_RL = sbd.getDistance(0); // Rear Left IR
		IRdis_RR = sbd.getDistance(1); // Rear Right IR
		IRdis_SR = sbd.getDistance(2); // Side Right IR
		USFront = sbd.getDistance(4); // Front UltraSonic
		USRear = sbd.getDistance(5); // Rear UltraSonic 
		
		//WheelEncoder
		Distance = sbd.getDistance(6); // WheeelEncoder Data (mm)
		
		//Status
		cout << " ===== Rear IRs difference: " << abs (IRdis_RL - IRdis_RR) << endl;
		cout << " ===== Side Left Infrared reading: " << IRdis_SL << endl;
 		cout << " ===== Rear Left Infrared reading: " << IRdis_RL << endl;
 		cout << " ===== Rear Right Infrared reading: " << IRdis_RR << endl;
		cout << " ===== Side Right Infrared reading: " << IRdis_SR << endl;
		cout << " ===== Front UltraSonic reading: " << USFront << endl;
		cout << " ===== Rear UltraSonic reading: " << USRear << endl;
		cout << " ===== WheeelEncoder Data (mm): " << Distance << endl;
		cout << " ===== Driving_Speed: " << driving_speed << endl;
		cout << " ===== DesiredSteeringWheelAngle: " << desiredSteeringWheelAngle << endl;
		cout << " ===== Parking spot length: "<< gapWidth << endl;
		cout << " ===== Ptime_takenIndicator: "<< time_takenIndicator << endl;
		cout<< "  ===== Angle :"<<parkAngle<<endl;
		
		cout << "========  REAched" << (CurrentDist1 + DesiredDistance2)  << endl;

// 		
		// Design your control algorithm here depending on the input data from above.
		switch (driving_state) {
		case DRIVE: {
			cout << "\t In drive mode" << endl;
			driving_speed = SpeedF2;
			desiredSteeringWheelAngle = -1;
			
			if ((USFront < SafeDistance && USFront > MinSafeDistance)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			}
			if ((IRdis_SL < IRMaxDist && IRdis_SL > IRMinDist)){ 
				driving_state = START_OBST;
			
			}
		}
			break;

		case START_OBST: {
			cout << "\t \t START_OBST mode" << endl;
			//driving_speed = 1;
			
			if ((USFront < SafeDistance && USFront > MinSafeDistance)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			  
			}

			if ((IRdis_SL >= IRMaxDist || IRdis_SL < IRMinDist)) {
				driving_state = POSSIBLE_SPOT;
				CurrentDistSpot = Distance;
			}
		}
			break;

		case POSSIBLE_SPOT: {
		  
// 			cout << "---- DIstance so far: " << Distance << endl;
			cout << "\t POSSIBLE_SPOT" << endl;;
			
			if ((USFront < SafeDistance && USFront > MinSafeDistance)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			}

			if(IRdis_SL < IRMaxDist && IRdis_SL > IRMinDist){
			  gapWidth = Distance - CurrentDistSpot;
			  //CurrentDistSpot2 = Distance;
			  
			if (isSmallGapSize == 1){
			  MinParkingDist = 500;
			}			  
			 if(gapWidth > MinParkingDist){
			   desiredSteeringWheelAngle=0;
			   CurrentDist = Distance;
			   driving_state = STOP_FOR_PARKING; 
			 }else{
			  driving_state = DRIVE; 
			 }
			}
			
			cout << "\t Parking spot length: "<< gapWidth << endl;
		}
			break;

		case STOP_FOR_PARKING: { 
			
			rightIndicator = true;
			cout << "\t STOP_FOR_PARKING" << endl;
// 			TimeStamp currentTime2;
// 			time_taken = (currentTime2.toMicroseconds() / 1000.0) - start_timer;
					
// 			cout << "++++++++++ Stoping timer: " << time_taken << endl;
			
			if ((USFront < SafeDistance && USFront > MinSafeDistance)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			  
			} 
			if (IRdis_SL < IRMaxDist && IRdis_SL > IRMinDist){ 
				desiredSteeringWheelAngle = -42;
			  
			 
				if (Distance > (CurrentDist + DesiredDistance1)) {  
	 				
					//parking(vc, vd);
					CurrentDist1 = Distance;
					driving_state = PARKING;
					driving_speed = Stop_Speed;
					

				}
			}
		}
			break;

		case PARKING: {
			cout << "========: PARKING (calling parking)"  << endl;
			parking();
		}
			break;
			
		case NO_POSSIBLE_PARKING_PLACE: {
		  
			rightIndicator = false;
			cout << "\t\t========  NO_POSSIBLE_PARKING_PLACE"  << endl;
			driving_speed = Stop_Speed;
			
			
		}
			break;
			
		default: {
			cout << "Non of these states" << endl;
			driving_speed = Stop_Speed;
			desiredSteeringWheelAngle = -1;
		}
		}

		// Create vehicle control data.

		// With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
		vc.setSpeed(driving_speed);

		// With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
		//double desiredSteeringWheelAngle = 0; // 4 degree but SteeringWheelAngle expects the angle in radians!
		vc.setSteeringWheelAngle(desiredSteeringWheelAngle);

		// You can also turn on or off various lights:
		vc.setBrakeLights(brakeIndicator);
		vc.setLeftFlashingLights(leftIndicator);
		vc.setRightFlashingLights(rightIndicator);

		// Create container for finally sending the data.
		Container c(Container::VEHICLECONTROL, vc);
		// Send container.
		getConference().send(c);
		
	}
	driving_speed = 0;
	return ModuleState::OKAY;
	}


void Driver::parking() {
	cout << "\t\t========:  parking()"  << endl;
	switch (parking_state) {

	case BACKWARDS_RIGHT: {
		rightIndicator = true;
		driving_speed = SpeedB2;
		desiredSteeringWheelAngle = 0;
	  	cout << "========  BACKWARDS_RIGHT"  << endl;
		if (Distance > (CurrentDist1 + DesiredDistance2)) {
			parking_state = BACKWARDS_LEFT;
			CurrentDist2 = Distance;
		} 
	}
		break;
		
	case BACKWARDS_LEFT: {
		
		driving_speed = SpeedB1;
		desiredSteeringWheelAngle = -42;
		cout << "\t========  BACKWARDS_LEFT"  << endl;
		if (((Distance > (CurrentDist2 + DesiredDistance3)) && ((IRdis_RL < 13 && IRdis_RL > 2) || (IRdis_RR < 13 && IRdis_RR > 2))) || (USRear < 10 && USRear > 1)) {			
// 			TimeStamp currentTimeB;
// 			start_timerB = currentTimeB.toMicroseconds() / 1000.0;
			parking_state = FORWARD_RIGHT;

		} 
	}
		break;

	case FORWARD_RIGHT: {
		driving_speed = SpeedF1;
		desiredSteeringWheelAngle = 42;
		cout << "\t\t========  FORWARD_RIGHT"  << endl;
		if ((abs (IRdis_RL - IRdis_RR)) < 1 && ((IRdis_RL < 15 && IRdis_RL > 2) && (IRdis_RR < 15 && IRdis_RR > 2))) {
			parking_state = STOP;
			TimeStamp currentTime5;
			start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;
		}

		else if (USFront < 10 && USFront > 1) {
			if (isSmallGapSize == 1 && MoreStates < DesiredMoreStates) {
				parking_state = BACK_AGAIN;
				MoreStates += 1;
			} else if ((isSmallGapSize == 0) || MoreStates < DesiredMoreStates){
// 				parking_state = STOP;
// 				TimeStamp currentTime5;
// 				start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;
				parking_state = BACK_AGAIN;
			}
		}

		break;

	case BACK_AGAIN: {
		driving_speed = SpeedB1;
		desiredSteeringWheelAngle = -42;
		cout << "\t========  BACK_AGAIN"  << endl;
		parkAngle = asin((IRdis_RL - IRdis_RR)/11.0); //11 = dist between IR's

		if ((((abs (IRdis_RL - IRdis_RR)) < 1) && ((IRdis_RL < 15 && IRdis_RL > 2) && (IRdis_RR < 15 && IRdis_RR > 2))) || (USRear < 10 && USRear > 1)){
		  //(Distance > (CurrentDist4 + DesiredDistance5 || (Distance < (CurrentDist4 + DesiredDistance5))
			TimeStamp currentTime5;
			start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;
			parking_state = STOP;
		}
		else if (USRear < 10 && USRear > 1){
			if(isSmallGapSize == 1 && MoreStates < DesiredMoreStates) {
				parking_state = FORWARD_RIGHT;
				MoreStates += 1;
			}else {
				parking_state = STOP;
				TimeStamp currentTime5;
				start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;
			}
		} 
	}

		break;

	case STOP:{
	  	driving_speed = Stop_Speed;
		desiredSteeringWheelAngle = -1;
		rightIndicator = false;
		cout << "\t\t========  STOP"  << endl;
		cout << "****  stop the car  ****" << endl;
		TimeStamp currentTime6;
		time_takenIndicator = (currentTime6.toMicroseconds() / 1000.0)- start_timerIndicator;
		if (time_takenIndicator < 4000)  { 
			rightIndicator = true;
			leftIndicator = true;
		}else {
		   parking_state = DONE;
		}
	}
		break;
		
	case DONE:{
		rightIndicator = false;
		leftIndicator = false;
		cout << "\t\t========  DONE"  << endl;
		driving_speed = Stop_Speed;
	}

		break;
	default: {

		cout << "Non of these states" << endl;
	}
	}
}
}
} // msv

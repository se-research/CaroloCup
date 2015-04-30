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

int minParkingDist;
int maxParkingDist;
int safeDistance;
int minSensorVal = 1;
int desiredDistance1; 
int desiredDistance2;
int desiredDistance3;
int desiredDistance4;
int desiredDistance5;
int speedF1;
int speedF2;
int speedB1;
int speedB2;
int stop_Speed;
int dist_IR_Max;
int dist_IR_Min;
int totalDistance;
int distStampGap;
int distStamp;
int distStamp1;
int distStamp2;
int dist_US_Front;
int dist_US_Rear;
int dist_IR_SRF;
int dist_IR_RL;
int dist_IR_RR;
int dist_IR_SR;
bool rightIndicator = false;
bool leftIndicator = false;
bool brakeIndicator = false;
bool initialized = false;
bool isSmallGapSize;
double parkAngle;
int gapLength = 0;


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
		minParkingDist = kv.getValue<uint32_t> ("driver.minParkingDist");
		maxParkingDist = kv.getValue<uint32_t> ("driver.maxParkingDist");
		safeDistance = kv.getValue<uint32_t> ("driver.safeDistance");
		desiredDistance1 = kv.getValue<uint32_t> ("driver.desiredDistance1");
		desiredDistance2 = kv.getValue<uint32_t> ("driver.desiredDistance2");
		desiredDistance3 = kv.getValue<uint32_t> ("driver.desiredDistance3");
		desiredDistance4 = kv.getValue<uint32_t> ("driver.desiredDistance4");
		desiredDistance5 = kv.getValue<uint32_t> ("driver.desiredDistance5");
		speedF1 = kv.getValue<uint32_t> ("driver.speedF1");
		speedF2 = kv.getValue<uint32_t> ("driver.speedF2");
		speedB1 = kv.getValue<uint32_t> ("driver.speedB1");
		speedB2 = kv.getValue<uint32_t> ("driver.speedB2");
		isSmallGapSize = kv.getValue<uint32_t> ("driver.isSmallGapSize");
// 		dist_IR_Max = kv.getValue<uint32_t> ("driver.dist_IR_Max");
// 		dist_IR_Min = kv.getValue<uint32_t> ("driver.dist_IR_Min");
		//cout << "***********  Sensor ID: " << m_sensorId << endl;	
		initialized = true;

		cout << "initilized!" << endl;
	}
	
// 	cout << "minParkingDist" << minParkingDist << endl;
// 	cout << "maxParkingDist" << maxParkingDist << endl;
// 	cout << "safeDistance" << safeDistance << endl;
// 	cout << "desiredDistance1" << desiredDistance1 << endl;
// 	cout << "desiredDistance2" << desiredDistance2 << endl;
// 	cout << "desiredDistance3" << desiredDistance3 << endl;
// 	cout << "desiredDistance4" << desiredDistance4 << endl;
// 	cout << "desiredDistance5" << desiredDistance5 << endl;
// 	cout << "speedF1" << speedF1 << endl;
// 	cout << "speedF2" << speedF2 << endl;
// 	cout << "speedB1" << speedB1 << endl;
// 	cout << "speedB2" << speedB2 << endl;
// 	cout << "isSmallGapSize" << isSmallGapSize << endl;
// 	cout << "dist_IR_Max" << dist_IR_Max << endl;
// 	cout << "dist_IR_Min" << dist_IR_Min << endl;


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
		dist_IR_SRF = sbd.getDistance(3); // Side Left IR // *on legendary is the Front-Side-Right
		dist_IR_RL = sbd.getDistance(0); // Rear Left IR
		dist_IR_RR = sbd.getDistance(1); // Rear Right IR
		dist_IR_SR = sbd.getDistance(2); // Side Right IR
		dist_US_Front = sbd.getDistance(4); // Front UltraSonic
		dist_US_Rear = sbd.getDistance(5); // Rear UltraSonic 
		
		//WheelEncoder
		totalDistance = sbd.getDistance(6); // WheeelEncoder Data (mm)
		
		//Status
		cout << " ===== Rear IRs difference: " << abs (dist_IR_RL - dist_IR_RR) << endl;
		cout << " ===== Side Left Infrared reading: " << dist_IR_SRF << endl;
 		cout << " ===== Rear Left Infrared reading: " << dist_IR_RL << endl;
 		cout << " ===== Rear Right Infrared reading: " << dist_IR_RR << endl;
		cout << " ===== Side Right Infrared reading: " << dist_IR_SR << endl;
		cout << " ===== Front UltraSonic reading: " << dist_US_Front << endl;
		cout << " ===== Rear UltraSonic reading: " << dist_US_Rear << endl;
		cout << " ===== WheeelEncoder Data (mm): " << totalDistance << endl;
		cout << " ===== Driving_Speed: " << driving_speed << endl;
		cout << " ===== DesiredSteeringWheelAngle: " << desiredSteeringWheelAngle << endl;
		cout << " ===== Parking spot length: "<< gapLength << endl;
		cout << " ===== Ptime_takenIndicator: "<< time_takenIndicator << endl;
		cout<< "  ===== Angle :"<<parkAngle<<endl;
		
		cout << "========  REAched" << (distStamp1 + desiredDistance2)  << endl;

// 		
		switch (driving_state) {
		case DRIVE: {
			cout << "\t In drive mode" << endl;
			driving_speed = speedF2;
			desiredSteeringWheelAngle = -1;
			
			if ((dist_US_Front < safeDistance && dist_US_Front > minSensorVal)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			}
			if ((dist_IR_SRF < dist_IR_Max && dist_IR_SRF > minSensorVal)){ 
				driving_state = START_OBST;
			
			}
		}
			break;

		case START_OBST: {
			cout << "\t \t START_OBST mode" << endl;
			//driving_speed = 1;
			
			if ((dist_US_Front < safeDistance && dist_US_Front > minSensorVal)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			  
			}

			if ((dist_IR_SRF > dist_IR_Max || dist_IR_SRF < minSensorVal)) {
				driving_state = POSSIBLE_SPOT;
				distStampGap = totalDistance;
			}
		}
			break;

		case POSSIBLE_SPOT: {
		  
// 			cout << "---- DIstance so far: " << totalDistance << endl;
			cout << "\t POSSIBLE_SPOT" << endl;;
			
			if ((dist_US_Front < safeDistance && dist_US_Front > minSensorVal)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			  
			}
			
			if(dist_IR_SRF < dist_IR_Max && dist_IR_SRF > minSensorVal){
			  gapLength = totalDistance - distStampGap;
			  //distStampGap2 = Distance;
			 if(gapLength > minParkingDist){
			   distStamp = totalDistance;
			   driving_state = STOP_FOR_PARKING; 
			 }else{
			  driving_state = DRIVE; 
			 }
			}
			
			cout << "\t Parking spot length: "<< gapLength << endl;
		}
			break;
			

		case STOP_FOR_PARKING: { 
			
			rightIndicator = true;
			cout << "\t STOP_FOR_PARKING" << endl;
			
			if ((dist_US_Front < safeDistance && dist_US_Front > minSensorVal)){ 
				driving_state = NO_POSSIBLE_PARKING_PLACE;
			  
			}
			if (totalDistance > (distStamp + desiredDistance1)) {  
 				
				//parking(vc, vd);
				distStamp1 = totalDistance;
				driving_state = PARKING;
				driving_speed = stop_Speed;
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
			driving_speed = stop_Speed;
			
			
		}
			break;
			
			
		default: {

			cout << "Non of these states" << endl;

			driving_speed = stop_Speed;
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
		driving_speed = speedB2;
		desiredSteeringWheelAngle = 42;
	  	cout << "========  BACKWARDS_RIGHT"  << endl;
		if (totalDistance > (distStamp1 + desiredDistance2)) {
			parking_state = BACKWARDS_LEFT;
			distStamp2 = totalDistance;
			
		} 
	}
		break;
		
	case BACKWARDS_LEFT: {
		
		driving_speed = speedB1;
		desiredSteeringWheelAngle = -42;
		cout << "\t========  BACKWARDS_LEFT"  << endl;
		if ((dist_IR_RL < 13 && dist_IR_RL > minSensorVal) || (dist_IR_RR < 13 && dist_IR_RR > minSensorVal)) {
		  parking_state = FORWARD_RIGHT;
		} 
	}
		break;
	

	case FORWARD_RIGHT: {
		driving_speed = speedF1;
		desiredSteeringWheelAngle = 42;
		cout << "\t\t========  FORWARD_RIGHT"  << endl;
		parkAngle = asin((dist_IR_RL - dist_IR_RR)/11.0); //11 = dist between IR's
		if (dist_US_Front < 12 && dist_US_Front > minSensorVal) {
		parking_state = BACK_AGAIN;
		}
	}

		break;
		

	case BACK_AGAIN: {
		driving_speed = speedB1;
		desiredSteeringWheelAngle = -42;
		cout << "\t========  BACK_AGAIN"  << endl;
		if ((((abs (dist_IR_RL - dist_IR_RR)) < 1) && ((dist_IR_RR < 15 && dist_IR_RR > minSensorVal))) || (dist_US_Rear < 10)){
		  //(totalDistance > (distStamp4 + desiredDistance5 || (Distance < (distStamp4 + desiredDistance5))
			TimeStamp currentTime5;
			start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;
			parking_state = STOP;
		}

	}

		break;

	case STOP:{
	  	driving_speed = stop_Speed;
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
		driving_speed = stop_Speed;
	}

		break;
	default: {

		cout << "Non of these states" << endl;
	}
	}
}

} // msv

/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/data/control/ForceControl.h"
#include "core/data/environment/VehicleData.h"
#include "core/base/LIFOQueue.h"
#include "SensorBoardData.h"
#include "parkingDriver.h"
#include "LaneDetectionData.h"
#include "SensorDetectionData.h"

#include <stdio.h>
#include <math.h>

namespace carolocup {

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;
using namespace core::data::environment;

double force;
double old_y = 0;
double old_degree = 0;
double aver[3] = { 0, 0, 0 };
int index = 0;
bool init_time=false;
double dis=0;// measure the distance for the parking
double temp=0;
bool parked = false;
parkingDriver::parkingDriver(const int32_t &argc, char **argv) :
	ConferenceClientModule(argc, argv, "parkingDriver") {
}

parkingDriver::~parkingDriver() {
}

void parkingDriver::setUp() {
	// This method will be call automatically _before_ running body().
}

void parkingDriver::tearDown() {
	// This method will be call automatically _after_ return from body().
}

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE parkingDriver::body() {

	core::base::LIFOQueue lifo;
	addDataStoreFor(lifo);

	const double MAXIMUM_SPEED = 0.75;
	double w = 0;
	double e = 0;
	double esum = 0;
	double x = 0;
	double Kp = 1.0;
	double Ki = 1.0;
	double angle;
	TimeStamp previousTime_str;
	TimeStamp previousTime_brk;

	enum DRIVER_STATES {
		RUNNING_STATE = 0, PARKING_STATE = 1
	} state;
	state = RUNNING_STATE;


	while (getModuleState() == ModuleState::RUNNING) {
		bool hasReceivedLaneDetectionData = false;
		bool hasReceivedSensorDetectionData = false;
		int sensor = 0;
		LaneDetectionData theData;

		SensorDetectionData theSensorDistance;

		while (!lifo.isEmpty() && !parked) {
			// Read the recently received container.
			Container con = lifo.pop();

			// Check the container type if it is one of our expected types.
			if ((con.getDataType() == Container::USER_DATA_1) && hasReceivedSensorDetectionData)  {
				// We have found our expected container.
				theData = con.getData<LaneDetectionData> ();
				hasReceivedLaneDetectionData = true;
				break;
			}
			// Check the container type if it is one of our expected types.
			if (con.getDataType() == Container::USER_DATA_2) {
			 // We have found our expected container.
			 theSensorDistance = con.getData<SensorDetectionData> ();
			 hasReceivedSensorDetectionData = true;
			}

		}
		// Clear the remaining data.
		lifo.clear();

		if (hasReceivedLaneDetectionData) {
			// Do something with your average attribute:
			angle = theData.getNumericalValue();
		}

		if (hasReceivedSensorDetectionData) {
		 // Do something with your average attribute:
		 sensor = theSensorDistance.getNumericalValue();

		 cerr << " Value from sensorboard "
		 << theSensorDistance.getNumericalValue() << endl;
		 }
		// Get most recent vehicle data:
		Container containerVehicleData = getKeyValueDataStore().get(
				Container::VEHICLEDATA);
		VehicleData vd = containerVehicleData.getData<VehicleData> ();
		
		ForceControl fc;
		
		if (sensor == 1) {
			state = PARKING_STATE;
			parked = true;
		}

		switch (state) {

		case RUNNING_STATE: {
			fc.setLeftFlashingLights(false);
			fc.setRightFlashingLights(false);
			// Angle adjustment
			double degree = angle - 90;
			cerr << "LANE" << endl;

			double y;
			int ko, po;
			if((int)degree > 0){
				ko = 11;
				po = 3;
			}
			if ((int)degree < 0) {
				ko = 11;
				po = 3;
			}
				

			y = pow ((degree + 2)/ko,po);


			// max and min values for steering force
			if(y > 30){
				y = 30;
				fc.setRightFlashingLights(true);
			}
			if(y < -19){
				y = -19;
				fc.setLeftFlashingLights(true);
			}

			//Storing the previous data in case of fall-back
			old_y = y;
			old_degree = degree;

			//Averaging steering force based on previous values
			if (index > 2) {
				index = 0;
			}
			aver[index] = y;
			index++;
			double sumForce = 0;
			for (int i = 0; i < 3; i++) {
				sumForce = sumForce + aver[i];
			}

			//Send forces
			fc.setSteeringForce(3);
			fc.setAccelerationForce(0.001);

			TimeStamp currentTime_brk;
			double timeStep_brk = (currentTime_brk.toMicroseconds()
					- previousTime_brk.toMicroseconds()) / (1000.0 * 1000.0);

			previousTime_brk = currentTime_brk;

			// PI control algorithm for speed.
			if (vd.getSpeed() > 0 || !vd.isSimulation()) {
				w = MAXIMUM_SPEED;
				x = vd.getSpeed();
				e = w - x;
				esum *= 0.8; // aging factor
				esum = esum + e;
				Kp = 1.0 + abs(force) / 15.0; //2.0 + abs(y)/2.0;
				Ki = 1.0; //4.0 + Kp * 4.0;
				double p = Kp * e;
				double i = Ki * timeStep_brk * esum;
				y = p + i;
				if (y > 1.7) {
					y = 1.7;
				}
				fc.setBrakeForce(abs(y));

				if ((angle > 95 || angle < 85) && vd.getSpeed() > 0.8){
					fc.setBrakeForce(4);
					fc.setBrakeLights(true);
				}
			}
			break;
		}
		case PARKING_STATE:
			
			 	fc.setSteeringForce(0);
				fc.setAccelerationForce(0);
				fc.setBrakeForce(6);
				cout<<"STOP"<<endl;
				fc.setBrakeLights(true);
				
			  /*}else	if(parkingCount > 25 &&parkingCount <= 28 ){//&& vd.getSpeed() > 0.005){// Brake
				parkingCount++;
			 	fc.setSteeringForce(0);
				fc.setAccelerationForce(-10);
				fc.setBrakeForce(6);
				cout<<"STOP"<<endl;
				fc.setBrakeLights(true);
				//cout<<"sss " <<vd.getSpeed()<<endl;
				
			  }else if(parkingCount > 28 && parkingCount <= 33){ // Move back and return right
			 	parkingCount++;
			 	fc.setSteeringForce(0);
				fc.setAccelerationForce(-10);
				fc.setBrakeForce(5.9);
				fc.setRightFlashingLights(true);
				//cout<<"Back *****  '" << fc.getBrakeForce() <<  endl;
								
			 }else if(parkingCount > 33 && parkingCount <= 45){ // Move back return left
			 	parkingCount++;
			 	fc.setSteeringForce(50);
				fc.setAccelerationForce(-10);
				fc.setBrakeForce(5.9);
				 fc.setRightFlashingLights(true);
				//cout<<"Back **LLL***  '" << fc.getBrakeForce() <<  endl;					
			 }else if(parkingCount > 45 && parkingCount <= 55 ){//&& vd.getSpeed() < 0.005 ){ // return left
			 	parkingCount++;
			 	fc.setSteeringForce(-50);
				fc.setAccelerationForce(-10);
				fc.setBrakeForce(5.9);
				//cout<<"return right   *****  '" << fc.getBrakeForce() <<  endl;
								
			 }else if(parkingCount > 55 && parkingCount <= 500 ){//&& vd.getSpeed() < 0.005) { // Stop
			 	parkingCount++;
			 	fc.setSteeringForce(0);
				fc.setAccelerationForce(0);
				fc.setBrakeForce(6);
				fc.setBrakeLights(true);
				//cout<<"STOP '" << fc.getBrakeForce() <<  endl;
												
			 }else if(parkingCount > 500){
				parkingOk=false;
				parkingCount=0;
				lanefollowing=true;				
			 }*/
			
			
			}
			// Create container.
			fc.setUserData(2000);
			Container c(Container::FORCECONTROL, fc);
			// Send container.
			getConference().send(c);
		
	}

	return ModuleState::OKAY;
}

} // carolocup


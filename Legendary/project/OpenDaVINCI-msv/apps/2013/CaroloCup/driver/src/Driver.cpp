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

namespace msv {

	using namespace std;
	using namespace core::base;
	using namespace core::data;
	using namespace core::data::control;
	using namespace core::data::environment;
	
	
	// Constructor
	Driver::Driver(const int32_t &argc, char **argv) :
		ConferenceClientModule(argc, argv, "Driver") {
	}
	// Destructor
	Driver::~Driver() {}

	void Driver::setUp() {
		// This method will be call automatically _before_ running body().
		speed = 0.4;
		oldCurvature = 0;
		controlGains[0] = 10;
		controlGains[1] = 20;
		controlGains[2] = 30;
		scaledLength = length*SCALE_FACTOR;
	}

	void Driver::tearDown() {
		// This method will be call automatically _after_ return from body().
	}

	// This method will do the main data processing job.
	ModuleState::MODULE_EXITCODE Driver::body() {
		float x1, x2, x3, x4, x5, y1, y2, y3, y4, y5, k1, k2, m1, m2, k_avg, m_avg;
		float angularErrorLeft, angularErrorRight;

		while (getModuleState() == ModuleState::RUNNING) {
			// In the following, you find example for the various data sources that are available:

			// 1. Get most recent vehicle data:
			//Container containerVehicleData = getKeyValueDataStore().get(Container::VEHICLEDATA);
			//VehicleData vd = containerVehicleData.getData<VehicleData> ();
			//cerr << "Most recent vehicle data: '" << vd.toString() << "'" << endl;

			// 2. Get most recent sensor board data:
			//Container containerSensorBoardData = getKeyValueDataStore().get(Container::USER_DATA_0);
			//SensorBoardData sbd = containerSensorBoardData.getData<SensorBoardData> ();
			//cerr << "Most recent sensor board data: '" << sbd.toString() << "'" << endl;

			// 3. Get most recent user button data:
			//Container containerUserButtonData = getKeyValueDataStore().get(Container::USER_BUTTON);
			//UserButtonData ubd = containerUserButtonData.getData<UserButtonData> ();
			//cerr << "Most recent user button data: '" << ubd.toString() << "'" << endl;

			// 4. Get most recent steering data as fill from lanedetector for example:
			Container containerLaneDetectionData = getKeyValueDataStore().get(Container::USER_DATA_1);
			LaneDetectionData ldd = containerLaneDetectionData.getData<LaneDetectionData> ();
			cerr << "Most recent lane detection data: '" << sd.toString() << "'" << endl;

			// The two lines are delivered in a struct containing two Vec4i objects (vector of 4 integers)
			lines = ldd.getLaneDetectionData();
			lineRight = lines.solidRight;
			lineLeft = lines.dashed;
			
			x1 = lineLeft[0]; y1 = lineLeft[1];
			x2 = lineLeft[2]; y2 = lineLeft[3];
			x3 = lineRight[0]; y3 = lineRight[1];
			x4 = lineRight[2]; y4 = lineRight[3];
			angularErrorLeft = atan(x1-x2, y1-y2);
			angularErrorRight = atan(x3-x4, y3-y4);
			angularError = (angularErrorLeft + angularErrorRight)/2;
			k1 = (y2-y1)/(x2-x1);
			k2 = (y4-y3)/(x3-x4);
			m1 = y1-k1*x1; m2 = y3-k2*x3;
			k_avg = (k1+k2)/2;
			m_avg = (m1+m2)/2;
			x5 = (scaledLength-2*m_avg)*k_avg/(2*(pow(k_avg,2)+1));
			y5 = scaledLength/2 - (scaledLength-2*m_avg)/(2*(pow(k_avg,2)+1));
			lateralError = sign(x5)*sqrt(pow(x5,2) + pow(y5+scaledLength/2,2));
			//Scale from pixels to meters
			lateralError = lateralError/SCALE_FACTOR;

			//Simple proportional control law, propGain needs to be updated
			desiredSteeringWheelAngle = lateralError*propGain;

			//A more advanced control law
			oldCurvature = curvature;
			curvature = steeringWheelAngle*ANGLE_TO_CURVATURE;
			deltaPath = speed/getFrequency();
			curvatureDifferential = (curvature-oldCurvature)/deltaPath;
			desiredSteeringWheelAngle = feedbackLinearizationController(lateralError, angularError, curvature, curvatureDifferential,
					steeringWheelAngle, speed, controlGains);

			// Create vehicle control data.
			VehicleControl vc;

			// Range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards), set constant speed
			vc.setSpeed(speed);

			// With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
			//double desiredSteeringWheelAngle = 4; // 4 degree but SteeringWheelAngle expects the angle in radians!
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

		return ModuleState::OKAY;
	}

	float feedbackLinearizationController(float lateralError, float angularError,
			float curvature, float curvatureDifferential, float steeringWheelAngle, float speed, float *controlGains) {
		float nominator = pow(cos(angularError), 3)*length*curvatureDifferential/pow((1-lateralError*curvature), 3);
		nominator = nominator + lateralError*sin(angularError)*pow(cos(angularError), 2)*curvatureDifferential*curvature/
				pow((1-lateralError*curvature), 2);
		nominator = nominator + sin(angularError)*pow(tan(steeringWheelAngle), 3)/length;
		nominator = nominator - sin(angularError)*cos(angularError)*tan(steeringWheelAngle)/(1-lateralError*curvature);
		nominator = nominator - curvature*sin(2*angularError)*tan(steeringWheelAngle)/(1-lateralError*curvature);
		nominator = nominator - sin(2*angularError)*cos(angularError)*pow(curvature, 2)/pow((1-lateralError*curvature), 2);
		nominator = nominator - controlGains[0]*lateralError;
		nominator = nominator - controlGains[1]*sin(angularError)*speed;
		nominator = nominator - controlGains[2]*pow(v, 2)*cos(angularError)*(tan(steeringWheelAngle)/length
				- curvature*cos(angularError)/(1-lateralError*curvature);
		float denominator = cos(angularError)*(1+pow(tan(steeringWheelAngle, 2));
		return nominator/denominator;
	}

} // msv

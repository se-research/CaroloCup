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
	// Define control parameters
	float propGain = 10;	//For P/PI/PID-controller
	float integrationTime;	//For PI/PID-controller
	float derivativeTime;	//For PD/PID-controller
	float filterTime;		//For PID-controller
	float controlGains[3] = {10,20,10};	//For feedback linearization controller
	float deltaPath, lateralError, angularError, steeringWheelAngle, curvature, curvatureDifferential, oldCurvature, speed;
	const float ANGLE_TO_CURVATURE = 2.5;
	
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
	}

	void Driver::tearDown() {
		// This method will be call automatically _after_ return from body().
	}

	// This method will do the main data processing job.
	ModuleState::MODULE_EXITCODE Driver::body() {

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
			Container containerSteeringData = getKeyValueDataStore().get(Container::USER_DATA_1);
			SteeringData sd = containerSteeringData.getData<SteeringData> ();
			cerr << "Most recent steering data: '" << sd.toString() << "'" << endl;

			// Design your control algorithm here depending on the input data from above.
			lateralError = sd.getExampleData();
			// Pseudo-code
			//float scaledLength = length/scaleFactor;
			// Assume: points (Point1, Point2, Point3, Point4)
			//float angularError = atan(Point1.x-Point2.x, Point1.y-Point2.y)
			//float k = (line1[3]-line1[1])/(line1[2]-line1[0]);
			//float kN = (Point2.x-Point1.x)/(Point1.y-Point2.y);
			//float x3 = (Point1.y-k*Point1.x+scaledLength/2)/(kN-k);
			//float y3 = kN*x3-scaledLength/2;
			//float lateralError = sqrt(pow(x3,2)+pow(y3+length/2,2));

			//Simple proportional control law, propGain needs to be updated
			desiredSteeringWheelAngle = lateralError*propGain;

			//A more advanced control law
			oldCurvature = curvature;
			curvature = steeringWheelAngle*ANGLE_TO_CURVATURE;
			deltaPath = speed*samplingTime;
			curvatureDifferential = (curvature-oldCurvature)/deltaPath;
			desiredSteeringWheelAngle = FeedbackLinearizationController(lateralError, angularError, curvature, curvatureDifferential,
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

	float FeedbackLinearizationController(float lateralError, float angularError,
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

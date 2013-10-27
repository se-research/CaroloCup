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
#include "core/base/LIFOQueue.h"

// Data structures from msv-data library:
#include "SensorBoardData.h"

#include "Driver.h"

namespace carolocup {

	using namespace std;
	using namespace core::base;
	using namespace core::data;
	using namespace core::data::control;
	using namespace core::data::environment;

	// Constructor
  Driver::Driver(const int32_t &argc, char **argv) :
    ConferenceClientModule(argc, argv, "Driver") ,
    m_hasReceivedLaneDetectionData(false) ,
    m_deltaPath(0) ,
    m_angularError(0) ,
    m_steeringWheelAngle(0) ,
    m_curvature(0) ,
    m_curvatureDifferential(0) ,
    m_oldCurvature(0) ,
    m_speed(0) ,
    m_lateralError(0) ,
    m_intLateralError(0) ,
    m_derLateralError(0) ,
    m_desiredSteeringWheelAngle(0) ,
    m_scaledLength(0) ,
    m_propGain(1) ,
    m_intGain(0.1) ,
    m_derGain(100) ,
    m_length(0.3) ,
    ANGLE_TO_CURVATURE(2.5) ,
    SCALE_FACTOR (1200) ,
    m_timestamp(0) ,
    m_leftLine(Vec4i(0,0,0,0)) ,
    m_rightLine(Vec4i(0,0,0,0)) ,
    m_dashedLine(Vec4i(0,0,0,0))
  {}

	// Destructor
	Driver::~Driver() {}

	void Driver::setUp() {
		// This method will be call automatically _before_ running body().
		m_speed = 0.4;
		m_oldCurvature = 0;
		m_controlGains[0] = 10;
		m_controlGains[1] = 20;
		m_controlGains[2] = 30;
		m_scaledLength = m_length*SCALE_FACTOR;
	}

	void Driver::tearDown() {
		// This method will be call automatically _after_ return from body().
	}

	// This method will do the main data processing job.
	ModuleState::MODULE_EXITCODE Driver::body() {
		core::base::LIFOQueue lifo;
		addDataStoreFor(lifo);
		float x1, x2, x3, x4, x5, y1, y2, y3, y4, y5, k1, k2, m1, m2, k_avg, m_avg;
		float angularErrorLeft, angularErrorRight;

		while (getModuleState() == ModuleState::RUNNING) {
			m_hasReceivedLaneDetectionData = false;
			LaneDetectionData ldd;
			while (!lifo.isEmpty()) {
				// Read the recently received container.
				Container con = lifo.pop();

				// Check the container type if it is one of our expected types.
				if (con.getDataType() == Container::USER_DATA_1) {
					// We have found our expected container.
					ldd = con.getData<LaneDetectionData> ();
					m_hasReceivedLaneDetectionData = true;
					break;
				}
			}
			lifo.clear();

			// The two lines are delivered in a struct containing two Vec4i objects (vector of 4 integers)
      Lines lines = ldd.getLaneDetectionData();
			m_rightLine = lines.rightLine;
			m_leftLine = lines.dashedLine;

			x1 = m_leftLine[0]; y1 = m_leftLine[1];
			x2 = m_leftLine[2]; y2 = m_leftLine[3];
			x3 = m_rightLine[0]; y3 = m_rightLine[1];
			x4 = m_rightLine[2]; y4 = m_rightLine[3];

      cout << endl;
      cout << "x1: " << x1;
      cout << ",x2: " << x2;
      cout << ",x3: " << x3;
      cout << ",x4: " << x4;
      cout << ",y1: " << y1;
      cout << ",y2: " << y2;
      cout << ",y3: " << y3;
      cout << ",y4: " << y4;
      cout << endl;

			angularErrorLeft = atan2(x1-x2, y1-y2);
			angularErrorRight = atan2(x3-x4, y3-y4);
			m_angularError = (angularErrorLeft + angularErrorRight)/2;
			k1 = (y2-y1)/(x2-x1);
			k2 = (y4-y3)/(x3-x4);
			m1 = y1-k1*x1; m2 = y3-k2*x3;
			k_avg = (k1+k2)/2;
			m_avg = (m1+m2)/2;
			x5 = (m_scaledLength-2*m_avg)*k_avg/(2*(pow(k_avg,2)+1));
			y5 = m_scaledLength/2 - (m_scaledLength-2*m_avg)/(2*(pow(k_avg,2)+1));
      m_derLateralError = m_lateralError;
			m_lateralError = sqrt(pow(x5,2) + pow(y5+m_scaledLength/2,2));
			if (x5 < 0) m_lateralError = -m_lateralError;
			//Scale from pixels to meters
      int sec = difftime(m_timestamp, clock());
			m_lateralError = m_lateralError/SCALE_FACTOR;
      if(m_timestamp != 0) {
        m_intLateralError = m_intLateralError + m_lateralError * sec;
        m_derLateralError = (m_lateralError - m_derLateralError) / sec;
      }
      m_timestamp = clock();
			//Simple proportional control law, propGain needs to be updated
      m_desiredSteeringWheelAngle = m_lateralError*m_propGain;
      m_desiredSteeringWheelAngle += m_intLateralError*m_intGain;
      m_desiredSteeringWheelAngle += m_derLateralError*m_derGain;

			//A more advanced control law
			//oldCurvature = m_curvature;
			//m_curvature = m_steeringWheelAngle*ANGLE_TO_CURVATURE;
			//m_deltaPath = cos(angularError)*m_speed/(1-m_lateralError*m_curvature)/getFrequency();
			//m_curvatureDifferential = (m_curvature-oldCurvature)/deltaPath;
			//m_desiredSteeringWheelAngle = feedbackLinearizationController();

			// Create vehicle control data.
			VehicleControl vc;

			// Range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards), set constant speed
			vc.setSpeed(m_speed);

			// With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
			//double desiredSteeringWheelAngle = 4; // 4 degree but m_SteeringWheelAngle expects the angle in radians!
			vc.setSteeringWheelAngle(m_desiredSteeringWheelAngle);

			// You can also turn on or off various lights:
			vc.setBrakeLights(false);
			vc.setLeftFlashingLights(false);
			vc.setRightFlashingLights(false);

			// Create container for finally sending the data.
			Container c(Container::VEHICLECONTROL, vc);
			// Send container.
			getConference().send(c);
		}

		return ModuleState::OKAY;
	}

	float Driver::feedbackLinearizationController() {
		float nominator = pow(cos(m_angularError), 3)*m_length*m_curvatureDifferential/pow((1-m_lateralError*m_curvature), 3);
		nominator = nominator + m_lateralError*sin(m_angularError)*pow(cos(m_angularError), 2)*m_curvatureDifferential*m_curvature/
				pow((1-m_lateralError*m_curvature), 2);
		nominator = nominator + sin(m_angularError)*pow(tan(m_steeringWheelAngle), 3)/m_length;
		nominator = nominator - sin(m_angularError)*cos(m_angularError)*tan(m_steeringWheelAngle)/(1-m_lateralError*m_curvature);
		nominator = nominator - m_curvature*sin(2*m_angularError)*tan(m_steeringWheelAngle)/(1-m_lateralError*m_curvature);
		nominator = nominator - sin(2*m_angularError)*cos(m_angularError)*pow(m_curvature, 2)/pow((1-m_lateralError*m_curvature), 2);
		nominator = nominator - m_controlGains[0]*m_lateralError;
		nominator = nominator - m_controlGains[1]*sin(m_angularError)*m_speed;
		nominator = nominator - m_controlGains[2]*pow(m_speed, 2)*cos(m_angularError)*(tan(m_steeringWheelAngle)/m_length)
				- m_curvature*cos(m_angularError)/(1-m_lateralError*m_curvature);
		float denominator = cos(m_angularError)*(1+pow(tan(m_steeringWheelAngle), 2));
		return nominator/denominator;
	}

} // msv

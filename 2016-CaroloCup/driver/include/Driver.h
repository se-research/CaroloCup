/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef DRIVER_H_
#define DRIVER_H_

#include "core/base/module/TimeTriggeredConferenceClientModule.h"

#include "GeneratedHeaders_AutomotiveData.h"
#include "GeneratedHeaders_CoreData.h"
#include "core/io/conference/ContainerConference.h"
#include "core/data/Container.h"
#include "core/base/KeyValueConfiguration.h"
#include "opencv2/opencv.hpp"
#include <LaneDetectionData.h>

namespace msv {

using namespace std;
using namespace automotive;
using namespace automotive::miniature;

enum DRIVING_STATE {
	DRIVE = 0,
	START_OBST = 1,
	POSSIBLE_SPOT = 2,
	STOP_FOR_PARKING = 3,
	PARKING = 4,
	NO_POSSIBLE_PARKING_PLACE = 5

};

enum PARKING_STATE {
	BACKWARDS_RIGHT = 0,
	BACKWARDS_LEFT = 1,
	FORWARD_RIGHT = 2,
	BACK_AGAIN = 3,
	STOP = 4,
	DONE = 5
};

// Define control parameters
float m_angularError;
float m_speed;
double m_lateralError;
double m_intLateralError;
double m_derLateralError;
float m_desiredSteeringWheelAngle;
float m_propGain;
float m_intGain;
float m_derGain;
double SCALE_FACTOR;    //For example, 12000 dpm (dots-per-meter)

int32_t m_timestamp;

cv::Vec4i m_leftLine;
cv::Vec4i m_rightLine;
cv::Vec4i m_dashedLine;

/**
 * This class is a skeleton to send driving commands to Hesperia-light's vehicle driving dynamics simulation.
 */
class Driver: public core::base::module::TimeTriggeredConferenceClientModule {
private:
	/**
	 * "Forbidden" copy constructor. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the copy constructor.
	 *
	 * @param obj Reference to an object of this class.
	 */
	Driver(const Driver &/*obj*/);

	/**
	 * "Forbidden" assignment operator. Goal: The compiler should warn
	 * already at compile time for unwanted bugs caused by any misuse
	 * of the assignment operator.
	 *
	 * @param obj Reference to an object of this class.
	 * @return Reference to this instance.
	 */
	Driver& operator=(const Driver &/*obj*/);

public:
	/**
	 * Constructor.
	 *
	 * @param argc Number of command line arguments.
	 * @param argv Command line arguments.
	 */
	Driver(const int32_t &argc, char **argv);

	virtual ~Driver();

	coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode body();

private:

	DRIVING_STATE driving_state;
	PARKING_STATE parking_state;


	virtual void setUp();

	virtual void tearDown();
	void parking();
    bool laneFollowing(LaneDetectionData *data);
    void calculateErr(CustomLine currLine, CustomLine goalLine, float *angError, double *latError);
    float calculateDesiredHeading(float oldLateralError);
};

} // msv

#endif /*DRIVER_H_*/

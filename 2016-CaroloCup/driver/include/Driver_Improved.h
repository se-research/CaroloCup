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


namespace msv {

using namespace std;
using namespace automotive;
using namespace automotive::miniature;
bool initialized = false;
	int32_t m_timestamp;
	float previous_error = 0;
	float derivative = 0;
	float integral = 0;
	int Kd = 1;

	int Kp = 1;
	int Ki = 1;
	float m_angularError;
	double m_lateralError;
	int calculateDesiredHeading (int oldLateralError);

	void calculateErr(CustomLine ,CustomLine ,float *, double *);





	float m_desiredSteeringWheelAngle;


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
	float calculateDesiredHeading (float oldLateralError);

	void calculateErr(CustomLine ,CustomLine ,float *, double *);

	//DRIVING_STATE driving_state;
	//PARKING_STATE parking_state;


	virtual void setUp();

	virtual void tearDown();
	void parking();
};

} // msv

#endif /*DRIVER_H_*/

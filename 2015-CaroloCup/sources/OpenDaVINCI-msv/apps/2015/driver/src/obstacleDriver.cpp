/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <stdio.h>
#include <math.h>
#include <sstream>
#include <fstream>

#include <pthread.h>

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"
#include "core/data/Constants.h"
#include "core/data/control/VehicleControl.h"
#include "core/data/environment/VehicleData.h"
#include "core/base/LIFOQueue.h"

// Data structures from msv-data library:

#include <pthread.h>
#include "obstacleDriver.h"

#define NUMBER_OF_SENSORS 7
#define NUMBER_OF_SAMPLES 5

bool overtakingNow = false;
bool overtakingDone = false;
bool obstacleDetected = false;
bool ObjDetectedFR = false;
bool leftBlink=false;
bool rightBlink=false;

int startDist = 0;

int arr[NUMBER_OF_SENSORS][NUMBER_OF_SAMPLES], cirK = 0;

namespace msv
{

  using namespace std;
  using namespace core::base;
  using namespace core::data;
  using namespace core::data::control;
  using namespace core::data::environment;

// Constructor
  obstacleDriver::obstacleDriver (const int32_t &argc, char **argv) :
      ConferenceClientModule (argc, argv, "Driver"), m_hasReceivedLaneDetectionData (
	  false), after_intersection (false), m_angularError (0), m_speed (0), m_lateralError (
	  0), m_intLateralError (0), m_derLateralError (0), m_desiredSteeringWheelAngle (
	  0), m_propGain (2.05), m_intGain (8.38), m_derGain (0.23), SCALE_FACTOR (
	  752 / 0.41), m_timestamp (0), m_leftLine (Vec4i (0, 0, 0, 0)), m_rightLine (
	  Vec4i (0, 0, 0, 0)), m_dashedLine (Vec4i (0, 0, 0, 0))
  {

    for (int i = 0; i < NUMBER_OF_SENSORS; i++)
      {
	for (int j = 0; j < NUMBER_OF_SAMPLES; j++)
	  {
	    arr[i][j] = 0;
	  }
      }
  }

// Destructor
  obstacleDriver::~obstacleDriver ()
  {
  }

  void
  obstacleDriver::setUp ()
  {
    // This method will be call automatically _before_ running body().
    m_speed = 6; //leave like this for test purpose
  }

  void
  obstacleDriver::tearDown ()
  {
    // This method will be call automatically _after_ return from body().
  }

  bool debug = false;
  int increaseSpeed = 0;

//TODO: Set indicator logic

// This method will do the main data processing job.
  ModuleState::MODULE_EXITCODE
  obstacleDriver::body ()
  {
    // Get configuration data.
    KeyValueConfiguration kv = getKeyValueConfiguration ();

    VehicleControl vc;
 /*   float last_steer = 0;
       double t_base;
       int steer_change = 1;
       int steer_change_timing = 200;
       bool steer_sign;
       int inters_max_steer;
       int inters_min_steer;*/
    bool first = true;
    while (getModuleState () == ModuleState::RUNNING)
      {

	LaneDetectionData ldd;
	SensorBoardData sbd;
	Container conUserData1 = getKeyValueDataStore ().get (
	    Container::USER_DATA_1);
	Container conUserData0 = getKeyValueDataStore ().get (
	    Container::USER_DATA_0);

	if ((conUserData1.getReceivedTimeStamp ().getSeconds ()
	    + conUserData1.getReceivedTimeStamp ().getFractionalMicroseconds ())
	    < 1)
	  {
	    cout << "New lap. Waiting..." << endl;
	    continue;
	  }
	if ((conUserData0.getReceivedTimeStamp ().getSeconds ()
	    + conUserData0.getReceivedTimeStamp ().getFractionalMicroseconds ())
	    < 1)
	  {
	    cout << "No Sensor board data" << endl;
	    continue;
	  }

	sbd = conUserData0.getData<SensorBoardData> ();
	ldd = conUserData1.getData<LaneDetectionData> ();
	if (first)
	  {
	    for (int i = 0; i < NUMBER_OF_SAMPLES; i++)
	      {
		int urF = sbd.getDistance (4);
		int irFR_side = sbd.getDistance (1);
		int irRR_side = sbd.getDistance (3);
		int irRR = sbd.getDistance (0);
		movingAvg (urF, 4);
		movingAvg (irFR_side, 1);
		movingAvg (irRR_side, 3);
		movingAvg (irRR,0);
	      }
	    first = false;
	  }

	m_propGain = 4.5; //4.5;//2.05;
	m_intGain = 0.5; //1.0;//8.39; //8.39;
	m_derGain = 0.23; //0.23;

	bool overtake = overtaking (sbd);
	bool res = laneFollowing (&ldd,overtake);
/*	//Check for intersection
	// This algo needs to be changed to work according
	// to the distance traveled in the intersection,
	// instead of the time.
	bool res = false;

	if (ldd.getLaneDetectionDataDriver ().roadState == NORMAL
	    && !after_intersection)
	  {
	   // cout << "NOrmal state" << endl;
	    res = laneFollowing (&ldd,overtake);
	  }
	else if (ldd.getLaneDetectionDataDriver ().roadState == INTERSECTION
	    && ldd.getLaneDetectionDataDriver ().confidenceLevel == 2
	    && !after_intersection)
	  {
	    //cout << "Slow down the car" << endl;
	  }
	else if (ldd.getLaneDetectionDataDriver ().roadState == INTERSECTION
	    && ldd.getLaneDetectionDataDriver ().confidenceLevel == 5
	    && !after_intersection)
	  {
	    //cout << "Found Intersection..." << endl;
	    after_intersection = true;
	    TimeStamp t_start;
	    m_timestamp = t_start.toMicroseconds ();
	    t_base = m_timestamp;
	    last_steer = 0;
	    if (last_steer < 0)
	      {
		steer_sign = false;
		inters_max_steer = abs (last_steer) / 2;
		vc.setSteeringWheelAngle (int16_t (0));
		Container c (Container::VEHICLECONTROL, vc);
		getConference ().send (c);
	      }
	    else
	      {
		steer_sign = true;
		inters_min_steer = (-1) * (last_steer / 2);
		vc.setSteeringWheelAngle (int16_t (0));
		Container c (Container::VEHICLECONTROL, vc);
		getConference ().send (c);
	      }

	  }
	else if (after_intersection)
	  {
	    TimeStamp t_stop;
	    double timeStep_now = (t_stop.toMicroseconds () - t_base) / 1000.0;

	    double timeStep_total = (t_stop.toMicroseconds () - m_timestamp)
		/ 1000.0;
	    cout << "TIme: " << timeStep_total << endl;
	    if (timeStep_total > 2000.0)
	      { //Cross intersect for 3 seconds
		res = laneFollowing (&ldd,overtake);
		after_intersection = false;
	      }
	    else
	      {
		if (timeStep_now > steer_change_timing)
		  {
		    t_base = t_stop.toMicroseconds ();
		    if (steer_sign)
		      { //positive steering
			cout << "Steering: " << last_steer - steer_change
			    << endl;
			while (last_steer >= inters_min_steer)
			  {
			    vc.setSteeringWheelAngle (
				int16_t (last_steer - steer_change));
			    last_steer = last_steer - steer_change;
			  }
		      }
		    else
		      { // negative steering
			cout << "Steering: " << last_steer + steer_change
			    << endl;
			while (last_steer <= inters_max_steer)
			  {
			    vc.setSteeringWheelAngle (
				int16_t (last_steer + steer_change));
			    last_steer = last_steer + steer_change;
			  }
		      }
		  }

		Container c (Container::VEHICLECONTROL, vc);
		getConference ().send (c);
		cout << "Crossing Intersection..." << endl;
	      }

	  }
	else
	  {
	    cout << "U not supposed to be here: "
		<< ldd.getLaneDetectionDataDriver ().roadState << "=>"
		<< after_intersection << endl;
	  } */

	if (!res)
	  {
	    cout << "Waiting..." << endl;
	    continue;
	  }

	stringstream speedStream, steeringAngleStream;

	float desSteering = m_desiredSteeringWheelAngle * 180 / M_PI;
	cout << "steeringAngle" << desSteering << endl;

	if (desSteering > 41)
	  desSteering = 42;
	if (desSteering < -41)
	  desSteering = -42;

	vc.setSteeringWheelAngle (int16_t (desSteering));

	int speedVal;
	//int runSpeed = 1565;
	speedVal = m_speed;
	if (abs (desSteering) < 4)
	  {
	    increaseSpeed++;
	  }
	else
	  {
	    increaseSpeed = 0;
	  }

	if (increaseSpeed >= 3 && increaseSpeed < 6)
	  {
	    speedVal = m_speed + 1;
	  }
	else if (increaseSpeed >= 6)
	  {
	    speedVal = m_speed + 2;
	  }

	vc.setSpeed (speedVal);

	if(overtake){
	    //add indicators

	}
	vc.setLeftFlashingLights(leftBlink);
	vc.setRightFlashingLights(rightBlink);

	Container c (Container::VEHICLECONTROL, vc);
	getConference ().send (c);
      }

    vc.setSpeed (0);
    vc.setSteeringWheelAngle (0);
    return ModuleState::OKAY;
  }
  bool
  obstacleDriver::overtaking (SensorBoardData sensorData)
  {
    int urF = sensorData.getDistance (4);
    int irFR_side = sensorData.getDistance (1);
    int irRR_side = sensorData.getDistance (3);
    int irR = sensorData.getDistance(0);
    float urF_avg = movingAvg (urF, 4);
    float irFR_avg = movingAvg (irFR_side, 1);
    float irRR_avg = movingAvg (irRR_side, 3);
    float irR_avg = movingAvg (irR,0);

    float timeToCollide=(urF_avg/m_speed)* 0.01; //assuming speed 0.6m/s, 1.5s at 90cm, 1s at 60cm, for 0.4m/s, 2.25s - 1.5s

	cout<<"urf:"<<urF_avg<<endl;
	cout<<"irFL:"<<irFR_avg<<endl;
	cout<<"irRL:"<<irRR_avg<<endl;
	cout<<"ObstacleDetected:"<<obstacleDetected<<endl;
    if (timeToCollide<1 && !obstacleDetected)
      {
	obstacleDetected = true;
	m_speed=0;
	leftBlink=false;
	rightBlink=false;
	//stop indicators
      }
    else if (obstacleDetected)
      {
	m_speed=3;
	leftBlink=true;
	//flash left
	cout<<"Obstacle Previously Detected"<<endl;
	if (abs (irFR_avg) < 20 && abs (irFR_avg) > 6 && !ObjDetectedFR)
	  {
	    ObjDetectedFR = true;
	    cout << "overtakingStarted" << endl;
	  }
	 if (abs (irRR_avg) < 20 && abs (irRR_avg) > 6 && !overtakingNow &&ObjDetectedFR)
	    {
	       //stopLeft flash
	       leftBlink=false;
		overtakingNow = true;
	    }
	if ( abs (irFR_avg) < 3 && overtakingNow) // Needs to be refined, its too random
	  {
	    rightBlink=true;
	    //flash right
	    overtakingDone = true;
	    m_speed=6;

	  }

	if (overtakingDone)
	  {
	    obstacleDetected = false;
	    overtakingNow = false;
	  }
	if(abs (irR_avg)<25 && abs (irR_avg)>4)
	  {
	    rightBlink=false;
	  }
      }
    return obstacleDetected;

  }
  float
  obstacleDriver::movingAvg (int sensorVal, int sensor)
  {
    float out = 0;

    arr[sensor][cirK] = sensorVal;
    cirK = (cirK + 1) % NUMBER_OF_SAMPLES;

    for (int i = 0; i < NUMBER_OF_SAMPLES; i++)
      {
	out += arr[sensor][i];
      }
    return out / NUMBER_OF_SAMPLES;
  }
  float
  obstacleDriver::calculateDesiredHeading (float oldLateralError)
  {
    float desiredHeading;
    float theta = m_angularError / 180 * M_PI;
    //Scale from pixels to meters
    m_lateralError = m_lateralError / SCALE_FACTOR;
    if (m_timestamp != 0)
      {
	TimeStamp now;
	int32_t currTime = now.toMicroseconds ();
	double sec = (currTime - m_timestamp) / (1000000.0);
	m_intLateralError = m_intLateralError
	    + m_speed * cos (theta) * m_lateralError * sec;
	if ((m_intLateralError > 2 * m_lateralError && m_lateralError > 0)
	    || (m_lateralError < 0 && m_intLateralError < 2 * m_lateralError))
	  {
	    m_intLateralError = 2 * m_lateralError;
	  }
	m_derLateralError = (m_lateralError - oldLateralError) / sec;
	//cout << endl;
	//cout << "  sec: " << sec;
      }
    TimeStamp now;
    m_timestamp = now.toMicroseconds ();
    //Simple proportional control law, propGain needs to be updated
    desiredHeading = m_lateralError * m_propGain;
    desiredHeading += m_intLateralError * m_intGain;
    desiredHeading += m_derLateralError * m_derGain;
    return desiredHeading;
  }

  bool
  obstacleDriver::laneFollowing (LaneDetectionData *data, bool overtake)
  {
    cout << "enteredLaneFollowing" << endl;
    LaneDetectionData ldd = *data;
    Lines lines = ldd.getLaneDetectionData ();

    LaneDetectorDataToDriver trajectoryData = ldd.getLaneDetectionDataDriver ();


    if (trajectoryData.noTrajectory)
      {
	cout << "No trajectory" << endl;
	return false;
      }

    if (debug)
      {
	cout << ",propGain: " << m_propGain;
	cout << ",intGain: " << m_intGain;
	cout << ",derGain: " << m_derGain;
	cout << endl;
      }

    float oldLateralError = m_lateralError;
    CustomLine goal;
    if (overtake)
      {
	goal = trajectoryData.leftGoalLines0;
      }
    else
      {
	goal = trajectoryData.rightGoalLines0;
      }

    calculateErr (trajectoryData.currentLine, goal, &m_angularError,
		  &m_lateralError);

    m_desiredSteeringWheelAngle = calculateDesiredHeading (oldLateralError);
    if (debug)
      {
	// cout << "  x_error: " << x_err;
	cout << "  derLateral: " << m_derLateralError;
	cout << "  intLateral: " << m_intLateralError;
	cout << "  lateral: " << m_lateralError;
	cout << "  orentation: " << m_angularError;
	// cout << "  theta: " << theta;
	cout << "  angle: " << m_desiredSteeringWheelAngle * 180 / M_PI;
	cout << "  speed: " << m_speed;

	cout << endl;
      }
    cout << "exit lane follwoing" << endl;
    return true;
  }

// float predictHeading(int time,float currSpeed, float currHeading)
// {
//     return;

// }

  void
  obstacleDriver::calculateErr (CustomLine currLine, CustomLine goalLine,
				float *angError, double *latError)
  {
    float x_goal = goalLine.p2.x;
    float x_pl = currLine.p2.x;

    float a = tan (goalLine.slope * M_PI / 180);
    float b = goalLine.p1.y - goalLine.p1.x * a;
    int x_coord = -b / a;
    x_goal = (x_coord + x_goal) / 2;
    float theta_avg = M_PI / 2;
    if (abs (x_goal - x_pl) > 0.001)
      {
	theta_avg = (0 - currLine.p2.y) / ((float) (x_goal - x_pl));
	theta_avg = atan (theta_avg);
      }
    if (theta_avg < 0)
      {
	theta_avg = 180 + (theta_avg * 180 / M_PI);
      }
    else
      {
	theta_avg = theta_avg * 180 / M_PI;
      }

    float theta_curr = currLine.slope;
    if (debug)
      {
	cout << "Position: " << x_pl << endl;
	cout << "Goal: " << x_goal << endl;
	cout << "Curr Orientation: " << theta_curr << endl;
	cout << "Goal Orientation: " << theta_avg << endl;
      }
    *angError = theta_avg - theta_curr;
    *latError = x_goal - x_pl;

    return;
  }

} // msv




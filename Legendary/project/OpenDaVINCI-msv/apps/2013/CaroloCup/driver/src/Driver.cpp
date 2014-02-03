/*
* Mini-Smart-Vehicles.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#include <stdio.h>
#include <math.h>
#include <sstream>

#include "core/io/ContainerConference.h"
#include "core/data/Container.h"
#include "core/data/Constants.h"
#include "core/data/control/VehicleControl.h"
#include "core/data/environment/VehicleData.h"
#include "core/base/LIFOQueue.h"

// Data structures from msv-data library:
#include "SensorBoardData.h"
#include "LidarData.h"
#include "SensorData.h"

#include "Driver.h"

namespace carolocup
{

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
    m_propGain(2.05) ,
    m_intGain(8.38) ,
    m_derGain(0.23) ,
    m_length(0.3) ,
    m_wheelRadius(0.27),
    m_protocol("/dev/ttyUSB0", 6),
    ANGLE_TO_CURVATURE(2.5) ,
    SCALE_FACTOR (752/0.41) ,
    m_timestamp(0) ,
    m_leftLine(Vec4i(0,0,0,0)) ,
    m_rightLine(Vec4i(0,0,0,0)) ,
    m_dashedLine(Vec4i(0,0,0,0))
{
    m_controlGains[0] = 8.0/3;
    m_controlGains[1] = 8.0/3;
    m_controlGains[2] = 8.0/3;
}

// Destructor
Driver::~Driver() {}

void Driver::setUp()
{
    // This method will be call automatically _before_ running body().
    m_speed = 0.55;
    m_oldCurvature = 0;
    m_controlGains[0] = 10;
    m_controlGains[1] = 20;
    m_controlGains[2] = 30;
    m_scaledLength = m_length*SCALE_FACTOR;
}

void Driver::tearDown()
{
    // This method will be call automatically _after_ return from body().
}

int __nsleep(const struct timespec *req, struct timespec *rem)
{
    struct timespec temp_rem;
    if(nanosleep(req,rem)==-1)
        __nsleep(rem,&temp_rem);
    else
        return 1;
}
 
int msleep(unsigned long milisec)
{
    struct timespec req={0},rem={0};
    time_t sec=(int)(milisec/1000);
    milisec=milisec-(sec*1000);
    req.tv_sec=sec;
    req.tv_nsec=milisec*1000000L;
    __nsleep(&req,&rem);
    return 1;
}

int16_t oldSteeringVal=0, oldSpeedVal=0;
int speedCnt = 0;
int16_t steeringVal=0;

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Driver::body()
{
    core::base::LIFOQueue lifo;
    addDataStoreFor(lifo);
    int x1, x2, x3, x4, y1, y2, y3, y4;
    cout << "Ready to go!" << endl;


/*
typedef struct {

  unsigned int readingIndex;
  unsigned int firstDegree;
  unsigned int firstDistance;
  unsigned int secondDegree;
  unsigned int secondDistance;
  unsigned int thirdDegree;
  unsigned int thirdDistance;
  unsigned int fourthDegree;
  unsigned int fourthDistance;
} lidarReading;

Lidar data is stored in a struct above and getting data from it
is done by "getData.getDistance().readingIndex" for the reading index e.t.c
*/



    while (getModuleState() == ModuleState::RUNNING)
    {
        m_hasReceivedLaneDetectionData = false;
        LaneDetectionData ldd;
	SensorData gatherData;
        LidarData getData;
        while (!lifo.isEmpty())
        {
            // Read the recently received container.
            Container con = lifo.pop();
            if (con.getDataType() == Container::USER_DATA_1)
            {
                // We have found our expected container.
                ldd = con.getData<LaneDetectionData> ();
                m_hasReceivedLaneDetectionData = true;
                break;
            }

           /*Getting the LidarData from the container with the ID USER_DATA_2*/
	   if (con.getDataType() == Container::USER_DATA_2)
            {
                // We have found our expected container.
                getData = con.getData<LidarData> ();
                cout<<"GOT LIDAR DATA FROM THE CONTAINER"<<endl;
                break;
            }

	   if (con.getDataType() == Container::USER_DATA_3)
            {
                // We have found our expected container.
                gatherData = con.getData<SensorData> ();
                cout<<"GOT SENSOR DATA FROM THE CONTAINER:   "<<gatherData.getUltrasonicDistance(2)<<endl;
                break;
		//lifo.clear();
            }


	    cout << "WAIT..." << endl;
        }
        lifo.clear();

        // The two lines are delivered in a struct containing two Vec4i objects (vector of 4 integers)
        Lines lines = ldd.getLaneDetectionData();
        if (lines.dashedLine[0] == 0 && lines.dashedLine[1] == 0 && lines.dashedLine[2] == 0 && lines.dashedLine[3] == 0) {
            m_leftLine = lines.leftLine;
        } else {
            m_leftLine = lines.dashedLine;
        }
        m_rightLine = lines.rightLine;
        
        m_propGain = 4.5;//4.5;//2.05;
        m_intGain = 2;//1.0;//8.39; //8.39;
        m_derGain = 0.4;//0.23;

        // Temporary solution to stop the car if a stop line is detected
        //if (lines.stopLineHeight != -1)
        //{
        //    m_speed = 0;
        //}
        int scr_width = lines.width;
        int scr_height = lines.height;

        x1 = m_leftLine[0];
        y1 = m_leftLine[1];
        x2 = m_leftLine[2];
        y2 = m_leftLine[3];
        x3 = m_rightLine[0];
        y3 = m_rightLine[1];
        x4 = m_rightLine[2];
        y4 = m_rightLine[3];

        if (( x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0 ) &&
                ( x3 == 0 && y3 == 0 && x4 == 0 && y4 == 0 ))
        {
            continue;
        }

        {
            cout << ",propGain: " << m_propGain;
            cout << ",intGain: " << m_intGain;
            cout << ",derGain: " << m_derGain;
            cout << endl;
        }

        float x_goal = lines.goalLine.p2.x;
        float x_pl = lines.currentLine.p2.x;
       
	float oldLateralError = m_lateralError;
	float a = tan(lines.goalLine.slope * M_PI / 180);
	float b = lines.goalLine.p1.y - lines.goalLine.p1.x * a;
	int x_coord = -b/a;
	x_goal = (x_coord + x_goal)/2;
	float theta_avg = M_PI/2;
    	if(abs(x_goal - x_pl) > 0.001)
    	{
        	theta_avg = (0 -lines.currentLine.p2.y) / ((float)(x_goal - x_pl));
        	theta_avg = atan(theta_avg);
    	}
    	if(theta_avg < 0)
    	{
        	theta_avg = 180 + (theta_avg*180/M_PI);
    	} else {
    		theta_avg = theta_avg*180/M_PI;
	}

	float theta_curr = lines.currentLine.slope;
	cout << "Position: " << x_pl << endl;
	cout << "Goal: " << x_goal << endl;
	cout << "Curr Orientation: " << theta_curr << endl;
	cout << "Goal Orientation: " << theta_avg << endl;
	m_angularError = theta_avg - theta_curr;
	float theta = m_angularError/180*M_PI;
	int x_err = x_goal - x_pl;
        m_lateralError = cos(theta) * x_err;

        //Scale from pixels to meters
        m_lateralError = m_lateralError/SCALE_FACTOR;

        if(m_timestamp != 0)
        {
            TimeStamp now;
            int32_t currTime = now.toMicroseconds();
            double sec = (currTime - m_timestamp) / (1000000.0);
	    m_intLateralError = m_intLateralError + m_speed * m_lateralError * sec;
	    if((m_intLateralError > 2*m_lateralError && m_lateralError > 0) || (m_lateralError < 0 && m_intLateralError < 2*m_lateralError)) {
		m_intLateralError = 2*m_lateralError;
            }
            m_derLateralError = (m_lateralError - oldLateralError) / sec;
            cout << endl;
            cout << "  sec: " << sec;
        }

        TimeStamp now;
        m_timestamp = now.toMicroseconds();

        //Simple proportional control law, propGain needs to be updated
        m_desiredSteeringWheelAngle = m_lateralError*m_propGain;
        m_desiredSteeringWheelAngle += m_intLateralError*m_intGain;
        m_desiredSteeringWheelAngle += m_derLateralError*m_derGain;

	cout << "  x_error: " << x_err;
        cout << "  derLateral: " << m_derLateralError;
        cout << "  intLateral: " << m_intLateralError;
        cout << "  lateral: " << m_lateralError;
        cout << "  orentation: " << m_angularError;
	cout << "  theta: " << theta;
        cout << "  angle: " << m_desiredSteeringWheelAngle;
        cout << "  speed: " << m_speed;
        cout << "  width " << scr_width;
        cout << "  height: " << scr_height;
        cout << endl;

        stringstream speedStream, steeringAngleStream;
	uint16_t speedVal = uint16_t((m_speed+0.5)/4.0*(1619-1523) + 1523);
	float angularSpeed = m_speed/m_wheelRadius * 10;
        uint16_t wheelFreqVal = uint16_t(angularSpeed);

	if(wheelFreqVal != oldSpeedVal) {
        	m_protocol.setWheelFrequency(wheelFreqVal);
	        cout << "Send wheel frequency: " << wheelFreqVal << endl;
	        oldSpeedVal = wheelFreqVal;
		speedCnt++;
	}
	if(speedCnt > 10) {
		oldSpeedVal = -1;
		speedCnt = 0;
	}
	msleep(2);

	float desSteering = m_desiredSteeringWheelAngle*180/M_PI;
	cout << "Desired steering: " << desSteering <<endl;
	if(desSteering > 43) desSteering = 43;
	if(desSteering < -43) desSteering = -43;

	int16_t steeringVal = int16_t(desSteering);
	if(steeringVal != oldSteeringVal) {
		cout << "Send angle: " << steeringVal << endl;
        	m_protocol.setSteeringAngle(steeringVal);
		oldSteeringVal = steeringVal;
	}
	msleep(1);
    }
    return ModuleState::OKAY;
}

float Driver::feedbackLinearizationController()
{
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

float Driver::feedbackLinearizationController2()
{
    float nominator = sin(m_angularError)*pow(tan(m_steeringWheelAngle), 3)/m_length;
    nominator -= sin(m_angularError)*cos(m_angularError)*tan(m_steeringWheelAngle);
    nominator -= m_controlGains[0]*m_lateralError;
    nominator -= m_controlGains[1]*sin(m_angularError)*m_speed;
    nominator -= m_controlGains[2]*pow(m_speed, 2)*cos(m_angularError)*(tan(m_steeringWheelAngle)/m_length)
                 - m_curvature*cos(m_angularError);
    float denominator = cos(m_angularError)*(1+pow(tan(m_steeringWheelAngle), 2));
    return nominator/denominator;
}

} // msv

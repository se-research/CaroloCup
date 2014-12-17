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
#include "laneDriver.h"

int indicators = -1;
bool indicatorsOn = false;

namespace msv
{

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;
using namespace core::data::environment;

// Constructor
laneDriver::laneDriver(const int32_t &argc, char **argv) :
    ConferenceClientModule(argc, argv, "Driver") ,
    m_hasReceivedLaneDetectionData(false) ,
    m_angularError(0) ,
    m_speed(0) ,
    m_lateralError(0) ,
    m_intLateralError(0) ,
    m_derLateralError(0) ,
    m_desiredSteeringWheelAngle(0) ,
    m_propGain(2.05) ,
    m_intGain(8.38) ,
    m_derGain(0.23) ,
    SCALE_FACTOR (752 / 0.41) ,
    m_timestamp(0) ,
    m_leftLine(Vec4i(0, 0, 0, 0)) ,
    m_rightLine(Vec4i(0, 0, 0, 0)) ,
    m_dashedLine(Vec4i(0, 0, 0, 0)) {}

// Destructor
laneDriver::~laneDriver() {}

void laneDriver::setUp()
{
    // This method will be call automatically _before_ running body().
    m_speed = (0.6 * 10); //leave like this for test purpose
}

void laneDriver::tearDown()
{
    // This method will be call automatically _after_ return from body().
}

bool debug = false;
int increaseSpeed = 0;


//TODO: Set indicator logic

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE laneDriver::body()
{
    // Get configuration data.
    KeyValueConfiguration kv = getKeyValueConfiguration();

    VehicleControl vc;


    while (getModuleState() == ModuleState::RUNNING)
        {

            LaneDetectionData ldd;
            Container conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);

            if ((conUserData1.getReceivedTimeStamp().getSeconds() + conUserData1.getReceivedTimeStamp().getFractionalMicroseconds()) < 1)
                {
                    cout << "Waiting..." << endl;
                }

            ldd = conUserData1.getData<LaneDetectionData>();

            m_propGain = 4.5;//4.5;//2.05;
            m_intGain = 0.5;//1.0;//8.39; //8.39;
            m_derGain = 0.23;//0.23;

            bool res = laneFollowing(&ldd);

            if (!res)
                {
                    cout << "Waiting..." << endl;
                    continue;
                }

            stringstream speedStream, steeringAngleStream;

            float desSteering = m_desiredSteeringWheelAngle * 180 / M_PI;

            if (desSteering > 41) desSteering = 42;
            if (desSteering < -41) desSteering = -42;

            vc.setSteeringWheelAngle(int16_t(desSteering));

            int speedVal;
            //int runSpeed = 1565;
            speedVal = m_speed;
            if (abs(desSteering) < 4)
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

            vc.setSpeed(speedVal);

            Container c(Container::VEHICLECONTROL, vc);
            getConference().send(c);
        }


    vc.setSpeed(0);
    vc.setSteeringWheelAngle(0);
    return ModuleState::OKAY;
}

bool laneDriver::laneFollowing(LaneDetectionData *data)
{
    int x1, x2, x3, x4, y1, y2, y3, y4;
    LaneDetectionData ldd = *data;
    // The two lines are delivered in a struct containing two Vec4i objects (vector of 4 integers)
    Lines lines = ldd.getLaneDetectionData();
    if (lines.dashedLine[0] == 0 && lines.dashedLine[1] == 0 && lines.dashedLine[2] == 0 && lines.dashedLine[3] == 0)
        {
            m_leftLine = lines.leftLine;
        }
    else
        {
            m_leftLine = lines.dashedLine;
        }
    m_rightLine = lines.rightLine;

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
            return false;
        }

    if (debug)
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
    int x_coord = -b / a;
    x_goal = (x_coord + x_goal) / 2;
    float theta_avg = M_PI / 2;
    if (abs(x_goal - x_pl) > 0.001)
        {
            theta_avg = (0 - lines.currentLine.p2.y) / ((float)(x_goal - x_pl));
            theta_avg = atan(theta_avg);
        }
    if (theta_avg < 0)
        {
            theta_avg = 180 + (theta_avg * 180 / M_PI);
        }
    else
        {
            theta_avg = theta_avg * 180 / M_PI;
        }

    float theta_curr = lines.currentLine.slope;
    if (debug)
        {
            cout << "Position: " << x_pl << endl;
            cout << "Goal: " << x_goal << endl;
            cout << "Curr Orientation: " << theta_curr << endl;
            cout << "Goal Orientation: " << theta_avg << endl;
        }
    m_angularError = theta_avg - theta_curr;
    float theta = m_angularError / 180 * M_PI;
    int x_err = x_goal - x_pl;
    m_lateralError = x_err;

    //Scale from pixels to meters
    m_lateralError = m_lateralError / SCALE_FACTOR;

    if (m_timestamp != 0)
        {
            TimeStamp now;
            int32_t currTime = now.toMicroseconds();
            double sec = (currTime - m_timestamp) / (1000000.0);
            m_intLateralError = m_intLateralError + m_speed * cos(theta) * m_lateralError * sec;
            if ((m_intLateralError > 2 * m_lateralError && m_lateralError > 0) || (m_lateralError < 0 && m_intLateralError < 2 * m_lateralError))
                {
                    m_intLateralError = 2 * m_lateralError;
                }
            m_derLateralError = (m_lateralError - oldLateralError) / sec;
            //cout << endl;
            //cout << "  sec: " << sec;
        }

    TimeStamp now;
    m_timestamp = now.toMicroseconds();

    //Simple proportional control law, propGain needs to be updated
    m_desiredSteeringWheelAngle = m_lateralError * m_propGain;
    m_desiredSteeringWheelAngle += m_intLateralError * m_intGain;
    m_desiredSteeringWheelAngle += m_derLateralError * m_derGain;

    if (debug)
        {
            cout << "  x_error: " << x_err;
            cout << "  derLateral: " << m_derLateralError;
            cout << "  intLateral: " << m_intLateralError;
            cout << "  lateral: " << m_lateralError;
            cout << "  orentation: " << m_angularError;
            cout << "  theta: " << theta;
            cout << "  angle: " <<  m_desiredSteeringWheelAngle * 180 / M_PI;
            cout << "  speed: " << m_speed;
            cout << "  width " << scr_width;
            cout << "  height: " << scr_height;
            cout << endl;
        }
    return true;
}


} // msv



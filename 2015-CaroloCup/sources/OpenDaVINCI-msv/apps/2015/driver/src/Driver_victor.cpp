
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
#include "Driver_victor.h"
#include "SteeringData.h"
#include "SensorBoardData.h"
#include "UserButtonData.h"

int indicators = -1;
bool indicatorsOn = false;

namespace msv
{

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;
using namespace core::data::environment;

int MinParkingDist = 500;
int MaxParkingDist = 650;
int SafeDistance = 30;
int Distance;
int CurrentDistSpot;
int CurrentDistSpot2;
int CurrentDist;
int DesiredDistance1 = 100; //700 is required 550+150;
int DesiredDistance2 = 650;
int DesiredDistance3 = 480;
int DesiredDistance4 = 90;
int DesiredDistance5 = 30;

int CurrentDist1;
int CurrentDist2;
int CurrentDist3;
int CurrentDist4;
int CurrentDist5;
int USFront;
int USRear;
int IRdis_SL;
int IRdis_RL;
int IRdis_RR;
int IRdis_SR;
bool rightIndicator = false;
bool leftIndicator = false;
bool brakeIndicator = false;
double start_timer;
double start_timer2;
double time_taken;
double time_taken2;
double start_timerB;
double time_takenB;
double start_timerIndicator;
double time_takenIndicator;
int driving_speed;          // Speed of the car
int desiredSteeringWheelAngle;// Angle of the wheels

//From Driver
bool debug = false;
int increaseSpeed = 0;

//Move aroound
int driving_state;
bool firstObject;
int secondDistance;


//To be removed
int SpeedF1 = 4;
int SpeedF2 = 6;
int SpeedB1 = -3;
int SpeedB2 = -7;

int parking_state;

// Constructor
Driver_victor::Driver_victor(const int32_t &argc, char **argv) :
    ConferenceClientModule(argc, argv, "Driver_victor") ,
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
Driver_victor::~Driver_victor() {}

void Driver_victor::setUp()
{
    // This method will be call automatically _before_ running body().
    m_speed = (0.6 * 10); //leave like this for test purpose

}

void Driver_victor::tearDown()
{
    // This method will be call automatically _after_ return from body().
}

//TODO: Set indicator logic

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Driver_victor::body()
{
    //TO BE REMOVED
    float desSteering;
    KeyValueConfiguration kv = getKeyValueConfiguration();
    firstObject = true;
    VehicleControl vc;

    while (getModuleState() == ModuleState::RUNNING)
        {
            LaneDetectionData ldd;
            Container conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);

            //Use of checkboxes
            Container containerVehicleControl = getKeyValueDataStore().get(
                                                    Container::VEHICLECONTROL);
            vc = containerVehicleControl.getData<VehicleControl>();

            Container containerSensorBoardData = getKeyValueDataStore().get(
                    Container::USER_DATA_0);

            SensorBoardData sbd =
                containerSensorBoardData.getData<SensorBoardData>();

            //Sensors
            IRdis_SL = sbd.getDistance(0); // Side Left IR
            IRdis_RL = sbd.getDistance(1); // Rear Left IR
            IRdis_RR = sbd.getDistance(2); // Rear Right IR
            IRdis_SR = sbd.getDistance(3); // Side Right IR
            USFront = sbd.getDistance(4); // Front UltraSonic
            USRear = sbd.getDistance(5); // Rear UltraSonic
            Distance = sbd.getDistance(6); // WheeelEncoder Data (mm)

            if ((conUserData1.getReceivedTimeStamp().getSeconds() + conUserData1.getReceivedTimeStamp().getFractionalMicroseconds()) < 1)
                {
                    cout << "Waiting..." << endl;
                }

            ldd = conUserData1.getData<LaneDetectionData>();

            m_propGain = 4.5;//4.5;//2.05;
            m_intGain = 0.5;//1.0;//8.39; //8.39;
            m_derGain = 0.23;//0.23;

            if (vc.getBrakeLights())
                {
                    driving_state = 1;
                }


            //Actual following
            switch (driving_state)
                {
                case 0:
                {
                    bool res = laneFollowing(&ldd);
                    if (!res)
                        {
                            cout << "Waiting..." << endl;
                            continue;
                        }

                    stringstream speedStream, steeringAngleStream;
                    desSteering = m_desiredSteeringWheelAngle * 180 / M_PI;
                    if (desSteering > 41) desSteering = 42;
                    if (desSteering < -41) desSteering = -42;

                    vc.setSteeringWheelAngle(int16_t(desSteering));
                }
                break;

                // do LD and search for open spots
                case 1:
                {
                    bool res = laneFollowing(&ldd);
                    if (!res)
                        {
                            cout << "Waiting..." << endl;
                            continue;
                        }

                    stringstream speedStream, steeringAngleStream;
                    desSteering = m_desiredSteeringWheelAngle * 180 / M_PI;
                    if (desSteering > 41) desSteering = 42;
                    if (desSteering < -41) desSteering = -42;

                    vc.setSteeringWheelAngle(int16_t(desSteering));

                    // in range
                    //compare distance if there is
                    if ((IRdis_SR < 25 || IRdis_SR > 2) && firstObject)
                        {
                            CurrentDistSpot = sbd.getDistance(6);
                            cerr << "Object found" << endl; 
                        }

                    //Second object, compare it here.

                    if ((IRdis_SR < 25 || IRdis_SR > 2) && !firstObject)
                        {
                            cerr << "Second Object found" << endl;                             
                            secondDistance = sbd.getDistance(6);
                            //compare the distance
                            //park or start over
                            if ((secondDistance - CurrentDist) > 640)
                                {
                                    CurrentDist = sbd.getDistance(6);
                                    driving_state = 2;
                                }
                            else
                                {
                                    firstObject = true;
                                }
                        }
                    //off range
                    if (IRdis_SR > 25 || IRdis_SR < 2)
                        {
                            cerr << "Nothing found" << endl;                             
                            firstObject = false;
                        }

                }

                break;

                //park then move to stop
                case 2:
                {
                    if (sbd.getDistance(6) - CurrentDist > 300)
                        {
                            parking();
                            driving_state = 3;
                        }
                }
                break;

                //catcher
                case 3:
                {
                    cout << "stop" << endl;
                }


                default:
                {
                    cout << "whatever error" << endl;
                }
                }

            //int speedVal;
            //int runSpeed = 1565;
            //speedVal = m_speed;
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
                    m_speed = + 1;
                }
            else if (increaseSpeed >= 6)
                {
                    m_speed = + 2;
                }
            //vc.setSpeed(speedVal);
            vc.setSpeed(m_speed);
            Container c(Container::VEHICLECONTROL, vc);
            getConference().send(c);

        }
    vc.setSpeed(0);
    vc.setSteeringWheelAngle(0);


    return ModuleState::OKAY;


}



bool Driver_victor::laneFollowing(LaneDetectionData *data)
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


void Driver_victor::parking()
{
    cout << "\t\t========:  parking()"  << endl;
    switch (parking_state)
        {
        case 0: //BACKWARDS 0
        {

            rightIndicator = true;
            driving_speed = -7;
            desiredSteeringWheelAngle = -40;
            cout << "========  BACKWARDS_RIGHT"  << endl;
            if (Distance > (CurrentDist1 + DesiredDistance2))
                {
                    parking_state = 1; //1
                    CurrentDist2 = Distance;

                }
        }
        break;

        //Confirm to use IR instead of US
        //Use one or two IR?
        case 1:
        {
            driving_speed = -3;
            desiredSteeringWheelAngle = 40;
            cout << "\t========  BACKWARDS_LEFT"  << endl;
            if ((Distance > (CurrentDist2 + DesiredDistance3)) || (IRdis_RL < 13 && IRdis_RL > 2) )
                {
                    TimeStamp currentTimeB;
                    start_timerB = currentTimeB.toMicroseconds() / 1000.0;
                    parking_state = 2;

                }
        }
        break;

        //Waiting to adjust  TO BE REMOVED
        case 2:
        {
            driving_speed = 0;
            cout << "\t WAIT_2" << endl;
            TimeStamp currentTimeB2;
            time_takenB = (currentTimeB2.toMicroseconds() / 1000.0) - start_timerB;

            cout << "++++++++++ Stoping timer: " << time_taken2 << endl;
            if (time_takenB > 300)
                {
                    cout << "++++++===== Stoping timerB: " << time_takenB << endl;
                    CurrentDist3 = Distance;
                    parking_state = 3;
                }
        }
        break;

        case 3:
        {
            driving_speed = 4;
            desiredSteeringWheelAngle = -40;
            cout << "\t\t========  FORWARD_RIGHT"  << endl;
            if ((Distance > (CurrentDist3 + DesiredDistance4)) || (USFront < 12 && USFront > 2))
                {
                    //(Distance > (CurrentDist3 + DesiredDistance4)) ||
                    parking_state = 4;
                    TimeStamp currentTime3;
                    start_timer2 = currentTime3.toMicroseconds() / 1000.0;
                }
        }

        break;

        //Waiting to adjust  TO BE REMOVED
        case 4:
        {
            driving_speed = 0;
            cout << "\t WAIT_3" << endl;
            TimeStamp currentTime4;
            time_taken2 = (currentTime4.toMicroseconds() / 1000.0) - start_timer2;

            cout << "++++++++++ Stoping timerF1: " << time_taken2 << endl;
            if (time_taken2 > 400)
                {
                    CurrentDist4 = Distance;
                    parking_state = 5;

                }

        }
        break;

        case 5:
        {
            driving_speed = -3;
            desiredSteeringWheelAngle = 40;
            cout << "\t========  BACK_AGAIN"  << endl;
            if (((abs (IRdis_RL - IRdis_RR)) < 1) && ((IRdis_RL < 15 && IRdis_RL > 2) || (IRdis_RR < 15 && IRdis_RR > 2)))
                {
                    //(Distance > (CurrentDist4 + DesiredDistance5)
                    parking_state = 6;
                    driving_speed = 3;
                    desiredSteeringWheelAngle = 0;
                    TimeStamp currentTime5;
                    start_timerIndicator = currentTime5.toMicroseconds() / 1000.0;

                }

        }

        break;

        //Just turn off indicator?
        case 6:
        {

            rightIndicator = false;
            cout << "\t\t========  STOP"  << endl;
            cout << "****  stop the car  ****" << endl;
            TimeStamp currentTime6;
            time_takenIndicator = (currentTime6.toMicroseconds() / 1000.0) - start_timerIndicator;
            if (time_takenIndicator < 3000)
                {
                    rightIndicator = true;
                    leftIndicator = true;
                }
            else
                {
                    parking_state = 7;
                }
        }

        // Parking completed
        case 7:
        {
            rightIndicator = false;
            leftIndicator = false;
            cout << "\t\t========  DONE"  << endl;
            driving_speed = 0;
        }
        break;

        default:
        {

            cout << "Non of these states" << endl;

            //driving_speed = 4;
            //desiredSteeringWheelAngle = 0;

        }
        }
}


} // msv



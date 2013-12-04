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
    m_protocol("/dev/ttyACM0", 8),
    ANGLE_TO_CURVATURE(2.5) ,
    SCALE_FACTOR (1200) ,
    m_timestamp(0) ,
    m_leftLine(Vec4i(0,0,0,0)) ,
    m_rightLine(Vec4i(0,0,0,0)) ,
    m_dashedLine(Vec4i(0,0,0,0))
{
    m_controlGains[0] = 8.0/3;
    m_controlGains[1] = 8.0/3;
    m_controlGains[2] = 8.0/3;
    /*
    const string SERIAL_PORT = "/dev/ttyACM0";
    const uint32_t SERIAL_SPEED =115200;
    m_serialPortPtr = core::wrapper::SerialPortFactory::createSerialPort(SERIAL_PORT, SERIAL_SPEED);
    m_protocol.setStringSender(m_serialPortPtr);
    m_protocol.setArduinoMegaDataListener(this);
    m_serialPortPtr->setPartialStringReceiver(&m_protocol);
    */
}

// Destructor
Driver::~Driver() {}

void Driver::setUp()
{
    // This method will be call automatically _before_ running body().
    m_speed = 0.4;
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

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Driver::body()
{
    //core::base::LIFOQueue lifo;
    //addDataStoreFor(lifo);
    int x1, x2, x3, x4, y1, y2, y3, y4;
    cout << "Ready to go!" << endl;
    while (getModuleState() == ModuleState::RUNNING)
    {
        m_hasReceivedLaneDetectionData = false;
        LaneDetectionData ldd;
        /*while (!lifo.isEmpty())
        {
            // Read the recently received container.
            Container con = lifo.pop();

            // Check the container type if it is one of our expected types.
            if (con.getDataType() == Container::USER_DATA_1)
            {
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
        m_propGain = lines.pGain / 10.0;
        m_intGain = lines.intGain;
        m_derGain = lines.derGain * 100;
        m_speed = lines.speed / 10.0;
        // Temporary solution to stop the car if a stop line is detected
        if (lines.stopLineHeight != -1)
        {
            m_speed = 0;
        }
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

        if (( x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0 ) ||
                ( x3 == 0 && y3 == 0 && x4 == 0 && y4 == 0 ) ||
                x1 == x2 || x3 == x4 )
        {
            continue;
        }

        {
            // for debug
            //x1 = 284;
            //x2 = 280;
            //x3 = 356;
            //x4 = 362;
            //y1 = 165;
            //y2 = 397;
            //y3 = 154;
            //y4 = 435;

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
            cout << ",propGain: " << m_propGain;
            cout << ",intGain: " << m_intGain;
            cout << ",derGain: " << m_derGain;
            cout << endl;
        }


        float theta1 = atan2(y1-y2, x1-x2);
        float theta2 = atan2(y3-y4, x3-x4);
        float s1 = x1 * cos(theta1) + y1 * sin(theta1);
        float s2 = x3 * cos(theta2) + y3 * sin(theta2);
        float intP1_x = (s1 - sin(theta1) * scr_height) / cos(theta1);
        float intP2_x =  (s2 - sin(theta2) * scr_height) / cos(theta2);
        float theta_avg = (theta1 + theta2) / 2;

        float x_goal = (intP1_x + intP2_x) /2 ;
        float x_pl = scr_width/2;
        m_lateralError = cos(theta_avg) * (x_goal - x_pl);
        m_angularError = M_PI_2 - theta_avg;

        //Scale from pixels to meters
        m_lateralError = m_lateralError/SCALE_FACTOR;

        if(m_timestamp != 0)
        {
            TimeStamp now;
            int32_t currTime = now.toMicroseconds();
            double sec = (currTime - m_timestamp) / (1000000.0);	//Why not 1.000000???
            m_intLateralError = m_intLateralError + m_speed * m_lateralError * cos(m_angularError) * sec;
            m_derLateralError = (m_lateralError - m_derLateralError) / sec;
            cout << endl;
            cout << "  sec: " << sec;
        }

        TimeStamp now;
        m_timestamp = now.toMicroseconds();
        //Simple proportional control law, propGain needs to be updated
        m_desiredSteeringWheelAngle = m_lateralError*m_propGain;
        m_desiredSteeringWheelAngle += m_intLateralError*m_intGain;
        m_desiredSteeringWheelAngle += m_derLateralError*m_derGain;

        cout << "  derLateral: " << m_derLateralError;
        cout << "  intLateral: " << m_intLateralError;

        //m_desiredSteeringWheelAngle = feedbackLinearizationController2();
        cout << "  lateral: " << m_lateralError;
        cout << "  angle: " << m_desiredSteeringWheelAngle;
        cout << "  speed: " << m_speed;
        cout << "  width " << scr_width;
        cout << "  height: " << scr_height;
        cout << endl;
        cout << endl;

        //A more advanced control law
        //oldCurvature = m_curvature;
        //m_curvature = m_steeringWheelAngle*ANGLE_TO_CURVATURE;
        //m_deltaPath = cos(angularError)*m_speed/(1-m_lateralError*m_curvature)/getFrequency();
        //m_curvatureDifferential = (m_curvature-oldCurvature)/deltaPath;
        //m_desiredSteeringWheelAngle = feedbackLinearizationController();

        // Create vehicle control data.
        //VehicleControl vc;

        // Range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards), set constant speed
        //vc.setSpeed(m_speed);

        // With setSteeringWheelAngle, you can steer in the range of -26 (left) .. 0 (straight) .. +25 (right)
        //double desiredSteeringWheelAngle = 4; // 4 degree but m_SteeringWheelAngle expects the angle in radians!
        //vc.setSteeringWheelAngle(m_desiredSteeringWheelAngle);

        // You can also turn on or off various lights:
        //vc.setBrakeLights(false);
        //vc.setLeftFlashingLights(false);
        //vc.setRightFlashingLights(false);

        // Create container for finally sending the data.
        //Container c(Container::VEHICLECONTROL, vc);
        // Send container.
        //getConference().send(c);*/
        m_desiredSteeringWheelAngle = 0.2;
        stringstream speedStream, steeringAngleStream;
	uint16_t speedVal = uint16_t((m_speed+2)/4.0*(1619-1523) + 1523);
        speedStream << 'm' << speedVal << '\0';
	cout << "Send speed: " << speedVal << endl;
        m_protocol.setSpeed(speedVal);
	uint16_t steeringVal = uint16_t(m_desiredSteeringWheelAngle*180/M_PI);
        steeringAngleStream << 's' << steeringVal << '\0';
	cout << "Send angle: " << steeringVal << endl;
        m_protocol.setSteeringAngle(steeringVal);
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

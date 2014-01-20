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
    m_wheelRadius(0.54),
    m_protocol("/dev/ttyACM0", 6),
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
    m_speed = 0.7;
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

    while (getModuleState() == ModuleState::RUNNING)
    {
        m_hasReceivedLaneDetectionData = false;
        LaneDetectionData ldd;
        LidarData getData;
        while (!lifo.isEmpty())
        {
            // Read the recently received container.
            Container con = lifo.pop();
	    //cout << "Container type: " << con.getDataType();
            // Check the container type if it is one of our expected types.
            if (con.getDataType() == Container::USER_DATA_1)
            {
                // We have found our expected container.
                ldd = con.getData<LaneDetectionData> ();
                m_hasReceivedLaneDetectionData = true;
                break;
		//lifo.clear();
            }

           /*Getting the LidarData from the container with the ID USER_DATA_2*/
	   if (con.getDataType() == Container::USER_DATA_2)
            {
                // We have found our expected container.
                getData = con.getData<LidarData> ();
                cout<<"GOT LIDAR DATA FROM THE CONTAINER"<<endl;
                break;
		//lifo.clear();
            }
	    cout << "WAIT..." << endl;
	    //lifo.clear();
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
        m_intGain = 0;//8.39; //8.39;
        m_derGain = 0.2;//0.23;

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
            /*cout << "x1: " << x1;
            cout << ",x2: " << x2;
            cout << ",x3: " << x3;
            cout << ",x4: " << x4;
            cout << ",y1: " << y1;
            cout << ",y2: " << y2;
            cout << ",y3: " << y3;
            cout << ",y4: " << y4;
            cout << endl;*/
            cout << ",propGain: " << m_propGain;
            cout << ",intGain: " << m_intGain;
            cout << ",derGain: " << m_derGain;
            cout << endl;
        }

       
        /*float theta1 = atan2(y1-y2, x1-x2);
        float theta2 = atan2(y3-y4, x3-x4);
        float s1 = x1 * cos(theta1) + y1 * sin(theta1);
        float s2 = x3 * cos(theta2) + y3 * sin(theta2);
        float intP1_x = (s1 - sin(theta1) * scr_height) / cos(theta1);
        float intP2_x =  (s2 - sin(theta2) * scr_height) / cos(theta2);
        //float theta_avg = (theta1 + theta2) / 2;
	cout << "P1.x: " << intP1_x << " P2.x: " << intP2_x << endl;*/

        float x_goal = lines.goalLine.p2.x;//intP1_x + 70;//(intP1_x + intP2_x) /2 - 70;
        float x_pl = lines.currentLine.p2.x;//scr_width/2 - 50;

/*	if(x_goal < x_pl && (x_goal + 100) > x_pl) {
	    x_goal = x_pl;
	}*/
       
	float oldLateralError = m_lateralError;
        //int x_error = (x_right + x_left - 752)/2;
	float a = tan(lines.goalLine.slope * M_PI / 180);
	float b = lines.goalLine.p1.y - lines.goalLine.p1.x * a;
	int x_coord = -b/a;
	x_goal = (x_coord + x_goal)/2;
	float theta_avg = M_PI/2;
    	if((x_goal - x_pl)!= 0)
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
        //float theta_avg = lines.goalLine.slope;//lines.supposedMidLine.slope;
	float theta_curr = lines.currentLine.slope;
	cout << "Position: " << x_pl << endl;
	cout << "Goal: " << x_goal << endl;
	//theta_curr = theta_curr + oldSteeringVal;
	cout << "Curr Orientation: " << theta_curr << endl;
	cout << "Goal Orientation: " << theta_avg << endl;
	m_angularError = theta_avg - theta_curr;
	/*if(steeringVal > 0) {
	    m_angularError = m_angularError + steeringVal/2;
        }*/
	float theta = m_angularError/180*M_PI;
	int x_err = x_goal - x_pl;
        m_lateralError = cos(theta) * x_err;

        //Scale from pixels to meters
        m_lateralError = m_lateralError/SCALE_FACTOR;

        if(m_timestamp != 0)
        {
            TimeStamp now;
            int32_t currTime = now.toMicroseconds();
            double sec = (currTime - m_timestamp) / (1000000.0);    //Why not 1.000000???
	    m_intLateralError = m_intLateralError + m_speed * m_lateralError * sec;
	    if(abs(m_intLateralError) > 2*abs(m_lateralError)) {
		m_intLateralError = 0;
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

        //m_desiredSteeringWheelAngle = feedbackLinearizationController2();
        cout << "  lateral: " << m_lateralError;
        cout << "  orentation: " << m_angularError;
	cout << "  theta: " << theta;
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
	//m_desiredSteeringWheelAngle=-0.2;
        stringstream speedStream, steeringAngleStream;
	uint16_t speedVal = uint16_t((m_speed+0.5)/4.0*(1619-1523) + 1523);
        uint16_t wheelFreqVal = uint16_t(m_speed/m_wheelRadius*10);
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
        //cout << "Send speed: " << speedVal << endl;
        //m_protocol.setSpeed(speedVal);
	msleep(2);
	//}
	float desSteering = m_desiredSteeringWheelAngle*180/M_PI;
	cout << "Desired steering: " << desSteering <<endl;
	if(desSteering > 32) desSteering = 32;
	if(desSteering < -32) desSteering = -32;
	int16_t steeringVal = int16_t(desSteering);
	if(steeringVal != oldSteeringVal) {
		cout << "Send angle: " << steeringVal << endl;
        	m_protocol.setSteeringAngle(steeringVal);
		oldSteeringVal = steeringVal;
	}
	msleep(1);
	//m_protocol.setBrakeForce('+');
	//}
	/*if(x_err > 200) {
		cout << "Cam angle: 10" << endl;
        	m_protocol.setCamAngle(10);
	} else if (x_err < -200) {
		cout << "Cam angle: -10" << endl;
		m_protocol.setCamAngle(-10);
	} else {
		cout << "Cam angle: 0" << endl;
		m_protocol.setCamAngle(0);
	}
	msleep(1);*/
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

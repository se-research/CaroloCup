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
#include "SensorBoardData.h"
#include "LidarData.h"
#include "SensorData.h"
#include "Sensors.h"
#include <pthread.h>
#include "Driver.h"

#define PARK_SPEED 0.5
#define REVERSE_SPEED 0.5

int lidarLookUp[360];

    
int sensorData[7];
bool inPSpot = false;
bool outPSpot = false;
int distanceTR = 0;
int pstate = 0; // 0 - searching; 1 - parking started; 3 - man1; 4 - man2; 5 - man3; 6 - finish
int reverseCnt = 0;
bool reverseDone = false;
bool driveReverse = false;
int indicators = -1;
bool indicatorsOn = false;

bool useRealSpeed = true;


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
    m_protocol("/dev/ttyACM1", 10),
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
    m_speed = 0.6;
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
int speedCnt = 0, steerCnt = 0;
int16_t steeringVal=0;
bool debug = false;
bool isParking = false;
bool isTestMode = false;
int increaseSpeed = 0;
bool isSpeedSent = false, isAngleSent = false, isIndicatorSent = false;

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE Driver::body()
{
    core::base::LIFOQueue lifo;
    addDataStoreFor(lifo);
    ofstream mylog;

    // Get configuration data.
    KeyValueConfiguration kv = getKeyValueConfiguration();
    isParking = kv.getValue<int32_t> ("driver.mode") == 2 ;
    debug = kv.getValue<int32_t> ("driver.debug") == 1;
    isTestMode = kv.getValue<int32_t> ("driver.mode") == 4;

    if(isTestMode) {
	mylog.open("test.log", ios::out | ios::app);
    }

    if(isParking) {
	m_speed = PARK_SPEED;
    }


    int i = 0;
    //int const ll = lidarLookUp[0][1];


    while (getModuleState() == ModuleState::RUNNING)
    {
        m_hasReceivedLaneDetectionData = false;
        LaneDetectionData ldd;
	SensorData gatherData;
        LidarData getData;

	if(isTestMode) {
		
		Container conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);
		if((conUserData1.getReceivedTimeStamp().getSeconds() + conUserData1.getReceivedTimeStamp().getFractionalMicroseconds()) < 1) {
			//conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);
			cout << "Waiting..." << endl;
				
		}	
		ldd = conUserData1.getData<LaneDetectionData>();

		Container conUserData3 = getKeyValueDataStore().get(Container::USER_DATA_3);
		gatherData = conUserData3.getData<SensorData>();
		if ((conUserData3.getReceivedTimeStamp().getSeconds() + conUserData3.getReceivedTimeStamp().getFractionalMicroseconds()) > 0) {
			//cout << "Received SensorData: " << gatherData.toString() << endl;
			sensorData[0] = gatherData.getInfraredDistance(1);
			sensorData[1] = gatherData.getInfraredDistance(2);
			sensorData[2] = gatherData.getInfraredDistance(3);
			sensorData[3] = gatherData.getInfraredDistance(4);
			sensorData[4] = gatherData.getUltrasonicDistance(1);
			sensorData[5] = gatherData.getUltrasonicDistance(2);
			sensorData[6] = gatherData.getMovement();
		}
		else {
			cout << "Still waiting for SensorData" << endl;
		}

		mylog << "[" << conUserData3.getReceivedTimeStamp().getSeconds() + conUserData3.getReceivedTimeStamp().getFractionalMicroseconds() << "]" << "   ";
		for(int i = 0; i < 7; i++) {
			if(i < 4) {
				mylog << "i" << i << "-" << sensorData[i] << ",";
			} else if (i >=4 && i < 6) {
				mylog << "u" << i-4 << "-" << sensorData[i] << ",";
			} else {
				mylog << "h-" << sensorData[i] << "." << endl;
			}
		}
		
	} else {
	
	if(!isParking) {
		Container conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);
		if((conUserData1.getReceivedTimeStamp().getSeconds() + conUserData1.getReceivedTimeStamp().getFractionalMicroseconds()) < 1) {
			//conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);
			cout << "Waiting..." << endl;
				
		}	
		ldd = conUserData1.getData<LaneDetectionData>();
		/*if ((conUserData1.getReceivedTimeStamp().getSeconds() + conUserData1.getReceivedTimeStamp().getFractionalMicroseconds()) > 0) {
			//cout << "Received LaneDetectionData: " << ldd.toString() << endl;
		}
		else {
			cout << "Still waiting for LaneDetectionData" << endl;
		}*/
	} else {

		/*Container conUserData2 = getKeyValueDataStore().get(Container::USER_DATA_2);
		getData = conUserData2.getData<LidarData>();
		if ((conUserData2.getReceivedTimeStamp().getSeconds() + conUserData2.getReceivedTimeStamp().getFractionalMicroseconds()) > 0) {
			//cout << "Received LidarData: " << getData.toString() << endl;
			for(int i = 0; i < 360; i ++) {
				cout << getData.data[i] << ",";
			}
			cout << endl;
		}
		else {
			cout << "Still waiting for LidarData" << endl;
		}*/

		Container conUserData3 = getKeyValueDataStore().get(Container::USER_DATA_3);
		gatherData = conUserData3.getData<SensorData>();
		if ((conUserData3.getReceivedTimeStamp().getSeconds() + conUserData3.getReceivedTimeStamp().getFractionalMicroseconds()) > 0) {
			//cout << "Received SensorData: " << gatherData.toString() << endl;
			sensorData[0] = gatherData.getInfraredDistance(1);
			sensorData[1] = gatherData.getInfraredDistance(2);
			sensorData[2] = gatherData.getInfraredDistance(3);
			sensorData[3] = gatherData.getInfraredDistance(4);
			sensorData[4] = gatherData.getUltrasonicDistance(1);
			sensorData[5] = gatherData.getUltrasonicDistance(2);
			sensorData[6] = gatherData.getMovement();
		}
		else {
			cout << "Still waiting for SensorData" << endl;
		}
	}
      
        m_propGain = 4.5;//4.5;//2.05;
        m_intGain = 0.5;//1.0;//8.39; //8.39;
        m_derGain = 0.23;//0.23;


	if(!isParking) {
		bool res = laneFollowing(&ldd);	
		if(!res) {
			cout << "Waiting..." << endl;
			continue;
		}
	} else {
		//cout << "Parking...";		
		bool res = parking();
	}


        stringstream speedStream, steeringAngleStream;
	float desSteering = m_desiredSteeringWheelAngle*180/M_PI;
	//cout << "Desired steering: " << desSteering <<endl;

	if(desSteering > 41) desSteering = 42;
	if(desSteering < -41) desSteering = -42;

	int16_t steeringVal = int16_t(desSteering);
	if(steeringVal != oldSteeringVal) {
		//cout << "Send angle: " << steeringVal << endl;
        	m_protocol.setSteeringAngle(steeringVal);
		isAngleSent = true;		
		oldSteeringVal = steeringVal;
		steerCnt ++;
	} else {
		isAngleSent = false;
		steerCnt++;
	}
	if(steerCnt > 20) {
		oldSteeringVal = -90;
		steerCnt = 0;
	}
	msleep(3);
	float speedVal;
	if(!isParking) {
		//int runSpeed = 1565;
		speedVal = m_speed;
		if(abs(desSteering) < 4) {
			increaseSpeed++;
		} else {
			increaseSpeed = 0;
		}
	
		if(increaseSpeed >= 3 && increaseSpeed < 6) {
			speedVal = m_speed + 0.1;
		} /*else if (increaseSpeed > 3 && increaseSpeed < 8) {
			speedVal = m_speed + (increaseSpeed - 3)*0.1;
		}*/ else if (increaseSpeed >= 6) {
			speedVal = m_speed + 0.2;
		}
	} else {
		speedVal = m_speed;
	}

	if(speedVal != oldSpeedVal) {
		if(isAngleSent) {
			msleep(10);
		}
		if(useRealSpeed) {
			cout << "Send wheel speed: " << speedVal << endl;
			m_protocol.setWheelFrequency((int)(speedVal*10), driveReverse);
		} else {
			cout << "Send speed: " << speedVal << endl;
			m_protocol.setSpeed(speedVal);
		}
		isSpeedSent = true;
		oldSpeedVal = speedVal;
		speedCnt++;
	} else {
		speedCnt++;
		isSpeedSent = false;
	}
	if(speedCnt > 120) {
		oldSpeedVal = -1;
		speedCnt = 0;
	}

	msleep(3);
	isIndicatorSent = false;
	switch(indicators) {
		case -1: {
			if(indicatorsOn) {
				m_protocol.setIndicatorsStop();
				indicatorsOn = false;
				isIndicatorSent = true;
			}
		} break;
		case 0: {
			m_protocol.setIndicatorsAll();
			indicatorsOn = true;
			isIndicatorSent = true;
		} break;
		case 1: {
			m_protocol.setIndicatorsRight();
			indicatorsOn = true;
			isIndicatorSent = true;
		} break;
		case 2: {
			m_protocol.setIndicatorsLeft();
			indicatorsOn = true;
			isIndicatorSent = true;
		} break;
		default: {}
	}
	//cout << isIndicatorSent << "," << isAngleSent << "," << isSpeedSent << endl;
	}
    }

    cout << "Body exiting............................................................................" << endl;
    if(!isTestMode) {
	    if(useRealSpeed) {
		    msleep(20);
		    m_protocol.setWheelFrequency(0, false);
	    } else { 
		    m_protocol.setSpeed(1520);
	    }
	    msleep(100);
	    m_protocol.setSteeringAngle(0);
	    msleep(100);
	    if(indicatorsOn) {
	    	m_protocol.setIndicatorsStop();
	    }
    } else {
	   mylog.close();
    }
    return ModuleState::OKAY;
}

bool Driver::laneFollowing(LaneDetectionData* data) {
        int x1, x2, x3, x4, y1, y2, y3, y4;
	LaneDetectionData ldd = *data;
        // The two lines are delivered in a struct containing two Vec4i objects (vector of 4 integers)
        Lines lines = ldd.getLaneDetectionData();
        if (lines.dashedLine[0] == 0 && lines.dashedLine[1] == 0 && lines.dashedLine[2] == 0 && lines.dashedLine[3] == 0) {
            m_leftLine = lines.leftLine;
        } else {
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

	if(debug)
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
	if(debug) {
		cout << "Position: " << x_pl << endl;
		cout << "Goal: " << x_goal << endl;
		cout << "Curr Orientation: " << theta_curr << endl;
		cout << "Goal Orientation: " << theta_avg << endl;
	}
	m_angularError = theta_avg - theta_curr;
	float theta = m_angularError/180*M_PI;
	int x_err = x_goal - x_pl;
        m_lateralError = x_err;

        //Scale from pixels to meters
        m_lateralError = m_lateralError/SCALE_FACTOR;

        if(m_timestamp != 0)
        {
            TimeStamp now;
            int32_t currTime = now.toMicroseconds();
            double sec = (currTime - m_timestamp) / (1000000.0);
	    m_intLateralError = m_intLateralError + m_speed * cos(theta) * m_lateralError * sec;
	    if((m_intLateralError > 2*m_lateralError && m_lateralError > 0) || (m_lateralError < 0 && m_intLateralError < 2*m_lateralError)) {
		m_intLateralError = 2*m_lateralError;
            }
            m_derLateralError = (m_lateralError - oldLateralError) / sec;
            //cout << endl;
            //cout << "  sec: " << sec;
        }

        TimeStamp now;
        m_timestamp = now.toMicroseconds();

        //Simple proportional control law, propGain needs to be updated
        m_desiredSteeringWheelAngle = m_lateralError*m_propGain;
        m_desiredSteeringWheelAngle += m_intLateralError*m_intGain;
        m_desiredSteeringWheelAngle += m_derLateralError*m_derGain;

	if(debug) {
		cout << "  x_error: " << x_err;
		cout << "  derLateral: " << m_derLateralError;
		cout << "  intLateral: " << m_intLateralError;
		cout << "  lateral: " << m_lateralError;
		cout << "  orentation: " << m_angularError;
		cout << "  theta: " << theta;
		cout << "  angle: " <<  m_desiredSteeringWheelAngle*180/M_PI; 
		cout << "  speed: " << m_speed;
		cout << "  width " << scr_width;
		cout << "  height: " << scr_height;
		cout << endl;
	}
	return true;
}

int prevIRVal = 0;
int accDistance = 0;
int revNeeded = 0;
int realDistance = 0;

bool Driver::parking() {
	switch(pstate) {
	case 0: {
		// Searching
		m_speed = PARK_SPEED;
		//useRealSpeed = true;
		m_desiredSteeringWheelAngle = 0;
		if(sensorData[3]==1 && sensorData[2] < 25 && !inPSpot) {
			cout << "In space " << sensorData[6] << endl;
			distanceTR = sensorData[6];
			inPSpot = true;
		} else if(sensorData[3] > 3 && sensorData[3] < 27 && (sensorData[2] == 1 || sensorData[2] > 27) && inPSpot) {
			cout << "Out space " << sensorData[6] << endl;
			realDistance = sensorData[6] - distanceTR;
			distanceTR = sensorData[6];
			cout << "Real distance: " <<  realDistance << endl;
			if(realDistance >= 12) {
				pstate = 1;
				m_speed = 0;
			}
			inPSpot = false;
		/*} else if( ((sensorData[3] > 3 && sensorData[3] <= 27) && inPSpot) {
			cout << "Space taken" << endl;*/
		} else if ((sensorData[3] > 3 && sensorData[3] <= 27) && (sensorData[2] > 3 && sensorData[2] <= 27) && !inPSpot) {
			cout << "Obstacle" << endl;
			inPSpot = false;
		} else if( (sensorData[3] < 3 || sensorData[3] > 27) && (sensorData[2] < 3 || sensorData[2] > 27) ) {
			cout << "Free" << endl;
		}
	}; break;
	case 1: {
		//Parking started
		cout << "Parking Started" << endl;
		cout << "Compensate Reverse" << endl;
		m_desiredSteeringWheelAngle = 0;
		//useRealSpeed = false;
		m_speed = REVERSE_SPEED;
		driveReverse = true;
		indicators = 1;
		/*if(!reverseDone) {
			reverseDone = doReverse();
		} else {*/
			if(accDistance == 0) {
				revNeeded = sensorData[6] - distanceTR;
				cout << "Needed reverse: " << revNeeded << endl;
				distanceTR = sensorData[6];
				cout << "dist: " << distanceTR << endl;
				accDistance = 1;
			} else {
				int diff = sensorData[6] - distanceTR;
				cout << "dist: " << distanceTR << endl;
				accDistance = accDistance + diff;
				distanceTR = sensorData[6];
			}
			cout << "acc: " << accDistance << endl;
			if(accDistance > revNeeded + 8) { //3
				accDistance = 0;
				pstate = 2;
				//m_speed = PARK_SPEED;
				//driveReverse = false;
				//reverseDone = false;
				indicators = 3;
			}
		//}
	}; break;
	case 2: {
		cout << "Forward Left" << endl;
		m_speed = PARK_SPEED;
		driveReverse = false;
		if(accDistance == 0) {
			distanceTR = sensorData[6];
			cout << "dist: " << distanceTR << endl;
			accDistance = 1;
		} else {
			int diff = sensorData[6] - distanceTR;
			cout << "dist: " << distanceTR << endl;
			accDistance = accDistance + diff;
			distanceTR = sensorData[6];	
			if(accDistance > 1) {
				m_desiredSteeringWheelAngle = -35;
			}
		}
		cout << "acc: " << accDistance << endl;
		if(accDistance > 7) {
			accDistance = 0;
			pstate = 3;
		}
		
	} break;
	case 3: {
		cout << "Backward straight" << endl;
		m_desiredSteeringWheelAngle = 0;
		m_speed = REVERSE_SPEED;
		driveReverse = true;
		/*if(!reverseDone) {
			reverseDone = doReverse();
		} else {*/
			if(accDistance == 0) {
				distanceTR = sensorData[6];
				cout << "dist: " << distanceTR << endl;
				accDistance = 1;
			} else {
				int diff = sensorData[6] - distanceTR;
				cout << "dist: " << distanceTR << endl;
				accDistance = accDistance + diff;
				distanceTR = sensorData[6];
			}
			cout << "acc: " << accDistance << endl;
			if(accDistance > 9) { //11
				accDistance = 0;
				pstate = 4;
				//reverseDone = false;
			}
		//}
	} break;
	case 4: {
		cout << "Backward Left" << endl;
		m_desiredSteeringWheelAngle = -35;
		m_speed = REVERSE_SPEED;
		if(sensorData[5] < 10 || (sensorData[0] > 3 && sensorData[0] < 6) || (sensorData[1] > 3 && sensorData[1] < 8) ) {
			pstate = 5;
			m_speed = 0;
		}
	} break;
	case 5: {
		cout << "Forward correction" << endl;
		m_speed = PARK_SPEED - 0.1;
		driveReverse = false;
		m_desiredSteeringWheelAngle = 10;
		//cout << sensorData[0] << "," << sensorData[1] << endl;
		if( (sensorData[1] > 3 && sensorData[0] > 3 && abs(sensorData[1] - sensorData[0]) < 2) || sensorData[4] < 17) {
			pstate = 6;
			indicators = -1;
		}
	}break;
	case 6: {
		//cout << sensorData[4] << ", " << sensorData[5] << endl;
		if(sensorData[4] < 10) {
			pstate=7;
		/*} else if (sensorData[5] < 10 || (sensorData[0] > 3 && sensorData[0] < 11) || (sensorData[1] > 3 && sensorData[1] < 11)) {
			pstate=5;*/
		} else {
			m_speed = 0;
			m_desiredSteeringWheelAngle = 0;
			cout << "FINISH" << endl;
			indicators = 0;
			pstate = 8;
		}
	} break;
	case 7: {
		cout << "Backward straight" << endl;
		m_desiredSteeringWheelAngle = 0;
		m_speed = REVERSE_SPEED - 0.1;
		driveReverse = true;
		/*if(!reverseDone) {
			reverseDone = doReverse();
		} else {*/
			if(accDistance == 0) {
				distanceTR = sensorData[6];
				cout << "dist: " << distanceTR << endl;
				accDistance = 1;
			} else {
				int diff = sensorData[6] - distanceTR;
				cout << "dist: " << distanceTR << endl;
				accDistance = accDistance + diff;
				distanceTR = sensorData[6];
			}
			cout << "acc: " << accDistance << endl;
			if(accDistance > 2) {
				accDistance = 0;
				pstate = 6;
				//reverseDone = false;
			}
		//}
	}
	default: {
	}
	}
	//cout << endl;
	return true;
}

bool Driver::doReverse() {
		cout << "Cnt: " << reverseCnt << endl;
		/*if(reverseCnt < 2) {
			m_speed = 1400;
		} else */
		if (reverseCnt < 3) {
			m_speed = 0;
		} else if (reverseCnt < 6) {
			m_speed = REVERSE_SPEED;
		/*} else if (reverseCnt < 8) {
			m_speed = 1520;
		} else if (reverseCnt == 10) {
			m_speed = REVERSE_SPEED;*/
			reverseCnt = 0;
			return true;	
		}
		reverseCnt++;
		return false;
}

} // msv



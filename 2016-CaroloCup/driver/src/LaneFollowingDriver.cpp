#include "LaneFollowingDriver.h"
#include "core/base/LIFOQueue.h"

float initialSpeed;
int increaseSpeed = 0;
int startBoxLength;
int initialDist;
int currDist;
bool firstRun;
float prevSteering;
int correctionDistance = 0;

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace automotive;
    using namespace automotive::miniature;

    LaneFollowingDriver::LaneFollowingDriver(const int32_t &argc, char **argv) :
            DriverGeneric(argc, argv),
            m_hasReceivedLaneDetectionData(false),
            after_intersection(false),
            m_angularError(0),
            m_speed(0),
            m_lateralError(0),
            m_intLateralError(0),
            m_derLateralError(0),
            m_desiredSteeringWheelAngle(0),
            m_propGain(2.05),
            m_intGain(8.38),
            m_derGain(0.23),
            SCALE_FACTOR(752.0 / 0.41),
            m_timestamp(0),
            m_leftLine(Vec4i(0, 0, 0, 0)),
            m_rightLine(Vec4i(0, 0, 0, 0)),
            m_dashedLine(Vec4i(0, 0, 0, 0)) { }

    LaneFollowingDriver::~LaneFollowingDriver() { }

    void LaneFollowingDriver::Routine() {
        LaneDetectionData ldd;
        Container conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);
        ldd = conUserData1.getData<LaneDetectionData>();
        Container containerSensorBoardData = getKeyValueDataStore().get(Container::USER_DATA_0);
        SensorBoardData sbd = containerSensorBoardData.getData<SensorBoardData>();
        if (sbd.containsKey_MapOfDistances(6)) currDist = (int) sbd.getValueForKey_MapOfDistances(6);

        if ((conUserData1.getReceivedTimeStamp().getSeconds() +
             conUserData1.getReceivedTimeStamp().getFractionalMicroseconds()) < 1) {
            cout << "New lap. Waiting..." << endl;
        }

        if (runStartBoxSequence && firstRun) {
            if ((containerSensorBoardData.getReceivedTimeStamp().getSeconds() +
                 containerSensorBoardData.getReceivedTimeStamp().getFractionalMicroseconds()) < 1) {
                cout << "no sbd..." << endl;
                return;
            }
            else {
                //initialDist=sbd.getDistance(6);//mm

                initialDist = -2;
                if (sbd.containsKey_MapOfDistances(6))
                    initialDist = (int) sbd.getValueForKey_MapOfDistances(6);

                firstRun = false;
                currDist = initialDist;
            }

        }
        //Start box logic
        LaneDetectorDataToDriver trajectoryData = ldd.getLaneDetectionDataDriver();
        if (runStartBoxSequence) {
            cout << "Start box: dist travelled" << currDist - initialDist << endl;
            DriverGeneric::desiredSteering = 0;
            DriverGeneric::desiredSpeed = m_speed;//we just the default

            //currDist=sbd.getDistance(6);

            cout << "Curr Distance" << currDist << endl;
            cout << "Initial Value" << initialDist << endl;
            //currDist++;//remove this
            if (currDist - initialDist < startBoxLength) {
                return;
            } else {
                runStartBoxSequence = false;
                cout << "Leaving start box" << endl;
            }
        }

        m_propGain = 4.5;//4.5;//2.05;
        m_intGain = 0.5;//1.0;//8.39; //8.39;
        m_derGain = 0.23;//0.23;

        bool res = laneFollowing(&ldd);

        m_speed = initialSpeed;

        if (! res) {
            cout << "Waiting..." << endl;
            DriverGeneric::desiredSpeed = initialSpeed;

            if (! correctionDistance) correctionDistance = (int) sbd.getValueForKey_MapOfDistances(6);

            DriverGeneric::desiredSteering = 0;

            if (currDist - correctionDistance > 5) {
                DriverGeneric::desiredSteering = 0;
                correctionDistance = -1;
            } else {
                DriverGeneric::desiredSteering = prevSteering;
            }

            return;
        }

        correctionDistance = 0;

        float desSteering = (float) (m_desiredSteeringWheelAngle * 180 / M_PI);

        if (desSteering > 41) desSteering = 42;
        if (desSteering < -41) desSteering = -42;

        desSteering *= 1;

        cout << "steeringAngle" << flush;
        cout << desSteering << endl;

        prevSteering = desSteering;
        DriverGeneric::desiredSteering = (float) (desSteering * M_PI / 180);

        float speedVal;

        speedVal = initialSpeed;

        if (abs(desSteering) < 4) speedVal = 1.6;

        cout << "Speed: " << speedVal << endl;
        cout << "SpeedDefault: " << initialSpeed << endl << endl;
        DriverGeneric::desiredSpeed = speedVal;
    }

    void LaneFollowingDriver::Initialize() {

        KeyValueConfiguration config = getKeyValueConfiguration();
        m_speed = config.getValue<float>("driver.realSpeed");
        initialSpeed = m_speed;
        cout << "speed" << m_speed << endl;

        //Startbox values
        startBoxLength = config.getValue<int32_t>("driver.startboxLength");
        initialDist = 0;
        currDist = 0;
        firstRun = true;
    }


    bool LaneFollowingDriver::laneFollowing(LaneDetectionData *data) {

        if (debug)
            cout << "enteredLaneFollowing" << endl;

        LaneDetectionData ldd = *data;

        LaneDetectorDataToDriver trajectoryData = ldd.getLaneDetectionDataDriver();

        if (trajectoryData.noTrajectory) {
            cout << "No trajectory" << endl;
            return false;
        }

        if (debug) {
            cout << ",propGain: " << m_propGain;
            cout << ",intGain: " << m_intGain;
            cout << ",derGain: " << m_derGain;
            cout << endl;
        }

        float oldLateralError = (float) m_lateralError;

        calculateErr(trajectoryData.currentLine, trajectoryData.rightGoalLines0, &m_angularError, &m_lateralError);

        m_desiredSteeringWheelAngle = calculateDesiredHeading(oldLateralError);
        if (debug) {
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

    void LaneFollowingDriver::calculateErr(CustomLine currLine, CustomLine goalLine, float *angError,
                                           double *latError) {
        float x_goal = goalLine.p2.x;
        float x_pl = currLine.p2.x;

        float a = (float) tan(goalLine.slope * M_PI / 180);
        float b = goalLine.p1.y - goalLine.p1.x * a;
        int x_coord = (int) (-b / a);
        x_goal = (x_coord + x_goal) / 2;
        float theta_avg = (float) (M_PI / 2);
        if (abs(x_goal - x_pl) > 0.001) {
            theta_avg = (0 - currLine.p2.y) / (x_goal - x_pl);
            theta_avg = atan(theta_avg);
        }
        if (theta_avg < 0) {
            theta_avg = (float) (180 + (theta_avg * 180 / M_PI));
        }
        else {
            theta_avg = (float) (theta_avg * 180 / M_PI);
        }

        float theta_curr = currLine.slope;
        if (debug) {
            cout << "Position: " << x_pl << endl;
            cout << "Goal: " << x_goal << endl;
            cout << "Curr Orientation: " << theta_curr << endl;
            cout << "Goal Orientation: " << theta_avg << endl;
        }
        *angError = theta_avg - theta_curr;
        *latError = x_goal - x_pl;

        return;
    }


    float LaneFollowingDriver::calculateDesiredHeading(float oldLateralError) {
        float desiredHeading;
        float theta = (float) (m_angularError / 180 * M_PI);
        //Scale from pixels to meters
        m_lateralError = m_lateralError / SCALE_FACTOR;
        if (m_timestamp != 0) {
            TimeStamp now;
            int32_t currTime = (int32_t) now.toMicroseconds();
            double sec = (currTime - m_timestamp) / (1000000.0);
            m_intLateralError = m_intLateralError
                                + m_speed * cos(theta) * m_lateralError * sec;
            if ((m_intLateralError > 2 * m_lateralError && m_lateralError > 0)
                || (m_lateralError < 0 && m_intLateralError < 2 * m_lateralError)) {
                m_intLateralError = 2 * m_lateralError;
            }
            m_derLateralError = (m_lateralError - oldLateralError) / sec;
            //cout << endl;
            //cout << "  sec: " << sec;
        }
        TimeStamp now;
        m_timestamp = (int32_t) now.toMicroseconds();
        //Simple proportional control law, propGain needs to be updated
        desiredHeading = (float) (m_lateralError * m_propGain);
        desiredHeading += m_intLateralError * m_intGain;
        desiredHeading += m_derLateralError * m_derGain;
        return desiredHeading;
    }
}

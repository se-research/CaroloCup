#include <stdio.h>
#include <math.h>
#include <sstream>
#include <fstream>

#include <pthread.h>

#include "core/io/conference/ContainerConference.h"
#include "core/data/Container.h"

#include "GeneratedHeaders_AutomotiveData.h"
#include "GeneratedHeaders_CoreData.h"

#include "core/base/LIFOQueue.h"

// Data structures from msv-data library:

#include <pthread.h>
#include <LaneDetectionData.h>


#include "Driver_Improved.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace automotive;
    using namespace automotive::miniature;
    float speed = 0.5;
    int desiredSteeringWheelAngle = 0;

    Driver::Driver(const int32_t &argc, char **argv) :
            TimeTriggeredConferenceClientModule(argc, argv, "Driver") { }

    Driver::~Driver() {
    }

    void Driver::setUp() {
        // This method will be call automatically _before_ running body().
    }

    void Driver::tearDown() {
        // This method will be call automatically _after_ return from body().
    }
    coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode Driver::body() {
        // Get configuration data.
        if (!initialized) {
            KeyValueConfiguration kv = getKeyValueConfiguration();
            initialized = true;
            cout << "initilized!" << endl;
        }
        VehicleControl vc;
        TimeStamp start;
        while (getModuleStateAndWaitForRemainingTimeInTimeslice() == coredata::dmcp::ModuleStateMessage::RUNNING) {

            Container containerSteeringData = getKeyValueDataStore().get(
                    Container::USER_DATA_1);
            SteeringData sd = containerSteeringData.getData<SteeringData>();
            LaneDetectionData ldd;
            Container conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);

            ldd = conUserData1.getData<LaneDetectionData>();
            LaneDetectorDataToDriver trajectoryData = ldd.getLaneDetectionDataDriver();

            //we still havent calculated the angular error
            calculateErr(trajectoryData.currentLine, trajectoryData.rightGoalLines0, &m_angularError, &m_lateralError);
            vc.setSteeringWheelAngle(int16_t(calculateDesiredHeading(m_angularError)));

        }
        return coredata::dmcp::ModuleExitCodeMessage::OKAY;
    }
    float  Driver::calculateDesiredHeading(float angularError) {
        float desiredHeading;
        TimeStamp now;
        int32_t currTime = now.toMicroseconds();
        int dt = (currTime - m_timestamp);
        integral = integral + (angularError * dt);
        derivative = (angularError - previous_error) / dt;
        desiredHeading = (Kp * angularError) + (Ki * integral) + (Kd * derivative);
        previous_error = angularError;
        return desiredHeading;
    }
    void Driver::calculateErr(CustomLine currLine, CustomLine goalLine, float *angError, double *latError) {
        float x_goal = goalLine.p2.x;
        float x_pl = currLine.p2.x;

        float a = tan(goalLine.slope * M_PI / 180);
        float b = goalLine.p1.y - goalLine.p1.x * a;
        int x_coord = -b / a;
        x_goal = (x_coord + x_goal) / 2;
        float theta_avg = M_PI / 2;
        if (abs(x_goal - x_pl) > 0.001) {
            theta_avg = (0 - currLine.p2.y) / ((float) (x_goal - x_pl));
            theta_avg = atan(theta_avg);
        }
        if (theta_avg < 0) {
            theta_avg = 180 + (theta_avg * 180 / M_PI);
        }
        else {
            theta_avg = theta_avg * 180 / M_PI;
        }
        float theta_curr = currLine.slope;
        *angError = theta_avg - theta_curr;
        *latError = x_goal - x_pl;
        return;
    }


}
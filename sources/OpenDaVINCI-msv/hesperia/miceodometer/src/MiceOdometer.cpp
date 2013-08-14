/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <limits>

#include <cstdlib>
#include <ctime>

#include "core/macros.h"

#include "core/data/Constants.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"

#include "core/data/environment/Point3.h"
#include "core/data/environment/VehicleData.h"
#include "hesperia/data/environment/EgoState.h"

#include "MiceOdometer.h"

namespace miceodometer {

    using namespace std;
    using namespace cv;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;
    using namespace core::data::environment;
    using namespace hesperia::data::environment;

    MiceOdometer::MiceOdometer(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "MiceOdometer"),
        D(2),
        m_phi(0),
        m_d(0),
        m_x(0),
        m_y(0),
        m_KF(),
        m_processNoise(0),
        m_measurementNoise(0),
        m_errorCovariance(0),
        m_estimatedX(0),
        m_estimatedY(0) {}

    MiceOdometer::~MiceOdometer() {}

    void MiceOdometer::setUp() {}

    void MiceOdometer::tearDown() {}

    void MiceOdometer::initializeKalmanFilter() {
        m_processNoise = 5e-2; // Influence of process errors: The smaller this parameter the greater is the distance between measured and estimated point.
        m_measurementNoise = 1e-4; // Influence of measurement errors: The smaller this parameter the greater is the influence of the measured points. 
        m_errorCovariance = 1e-1;

        m_KF = KalmanFilter(2 /*Number of dynamic parameters.*/, 2 /*Number of measured parameters.*/, 0 /*Number of control parameters.*/),

        m_KF.statePre.at<float>(0) = m_x; // Initialize the estimated global X position.
        m_KF.statePre.at<float>(1) = m_y; // Initialize the estimated global Y position.
        m_KF.transitionMatrix = Mat::eye(2,2,CV_32F); // The transition matrix is in our case: (1 0; 0 1), i.e. the parameters do not influence each other.

        setIdentity(m_KF.measurementMatrix);
        setIdentity(m_KF.processNoiseCov, Scalar::all(m_processNoise));
        setIdentity(m_KF.measurementNoiseCov, Scalar::all(m_measurementNoise));
        setIdentity(m_KF.errorCovPost, Scalar::all(m_errorCovariance));
    }

    void MiceOdometer::estimatePosition() {
        // Predict the next state.
        Mat predict = m_KF.predict();

        // Measurements are in our case the calculated position (m_x, m_y).
        Mat measurement(2, 1, CV_32F);
        measurement.at<float>(0) = m_x;
        measurement.at<float>(1) = m_y;

        // Correct Kalman filter by measurement.
        Mat estimation = m_KF.correct(measurement);

        // Get estimated position.
        m_estimatedX = estimation.at<float>(0);
        m_estimatedY = estimation.at<float>(1);
    }

    void MiceOdometer::calculatePosition(const double &deltaXLeft, const double &deltaYLeft, const double &deltaXRight, const double &deltaYRight, const double &timeStep, const double &direction) {
        // Algorithm for measuring and integrating driven distance and heading from two optical mice.

        const double lengthLeft = sqrt(deltaXLeft * deltaXLeft + deltaYLeft * deltaYLeft);
        const double lengthRight = sqrt(deltaXRight * deltaXRight + deltaYRight * deltaYRight);

        cout << "lengthLeft = " << lengthLeft << ", lengthRight = " << lengthRight << endl;

        // These two variables will contain the increments for the estimated heading and estimated driven path.
        double dotD = 0;
        double dotPhi = 0;
        
        if (fabs(lengthLeft - lengthRight) < 1e-5) {
            // Case: moving straight forward.
            cout << "Case: moving straight forward." << endl;

            dotD = lengthLeft * timeStep;
            // TODO for paper: compare with: dotD = ((lengthLeft + lengthRight)/2.0) * timeStep;
            // TODO for paper: compare with weighted lengthLeft and lengthRight depending on where to come from with aging factor.

            dotPhi = 0;
        }
        else if (lengthLeft > lengthRight) {
            // Case: turning to right.
            cout << "Case: turning to right." << endl;

            const double X = D / ( (lengthLeft/lengthRight) - 1 );

            // Calculate length of the perpendicular hX without being dependent on the mouse's coordinate frame.
            const double t1 = (2*X*X - lengthRight*lengthRight)/(2*X);
            const double hX = sqrt(X*X - t1*t1);

            const double PHI = asin(hX / X);
            dotD = (X + 0.5*D) * PHI * timeStep;
            dotPhi = (-1) * PHI * timeStep;
        }
        else if (lengthLeft < lengthRight) {
            // Case: turning to left.
            cout << "Case: turning to left." << endl;

            const double X = D / ( (lengthRight/lengthLeft) - 1 );

            // Calculate length of the perpendicular hX without being dependent on the mouse's coordinate frame.
            const double t1 = (2*X*X - lengthLeft*lengthLeft)/(2*X);
            const double hX = sqrt(X*X - t1*t1);

            const double PHI = asin(hX / X);
            dotD = (X + 0.5*D) * PHI * timeStep;
            dotPhi = PHI * timeStep;
        }

        // Correction factor for simulation (not accessible/required on real car --> validate with real mice):
        dotD *= getFrequency();
        dotPhi *= getFrequency();

        // Consider forwards/backwards driving (TODO: Derive direction from mice input).
        dotPhi *= direction;

        // Integrate readings over time to get globally driven path and global heading.
        m_d += dotD;
        m_phi += dotPhi;

        // Integrate calculated position over time and consider forwards/backwards driving.
        const double dotX = dotD * cos(m_phi) * direction;
        const double dotY = dotD * sin(m_phi) * direction;
        m_x += dotX;
        m_y += dotY;

        // Map phi into range 0..2pi.
        while (m_phi < 0) {
            m_phi += 2 * Constants::PI;
        }
        while (m_phi > 2 * Constants::PI) {
            m_phi -= 2 * Constants::PI;
        }

        cout << "dotD = " << dotD << ", dotPhi = " << dotPhi << endl;
    }

    ModuleState::MODULE_EXITCODE MiceOdometer::body() {
        KeyValueDataStore &kvs = getKeyValueDataStore();

        // Initialize random seed to simulate noise.
        srand(time(NULL));

        bool init = true;
        TimeStamp prevTime;
        EgoState prevEgoState;
        double egostateD = 0;

        // This parameter needs to be defined in the configuration: What is the initial global rotation of the car?
        double INITIAL_ROTATION_CAR = 0;

        const double ROT_LEFT = 90.0 * Constants::DEG2RAD; // Left mouse is mounted pi/4 to the left from the vehicle's heading.
        const double ROT_RIGHT = -90.0 * Constants::DEG2RAD; // Right mouse is mounted pi/4 to the right from the vehicle's heading.

        // Data from previous cycle.
        Point3 prevPositionLeftMouse;
        Point3 prevPositionRightMouse;

        // Data from current cycle.
        Point3 currPositionLeftMouse;
        Point3 currPositionRightMouse;

        double direction = 1; // < 0 backwards, >0 forwards.

        while (getModuleState() == ModuleState::RUNNING) {
            TimeStamp currentTime;
            double timeStep = (currentTime.toMicroseconds() - prevTime.toMicroseconds()) / (1000.0 * 1000.0);

            // Get current ego state for simulating virtually mounted mice.
            Container c = kvs.get(Container::EGOSTATE);
            EgoState es = c.getData<EgoState>();

            // Get driving direction (not directly accessible on real car).
            c = kvs.get(Container::VEHICLEDATA);
            VehicleData vd = c.getData<VehicleData>();
            direction = (vd.getSpeed() > 0) ? 1 : ((vd.getSpeed() < 0) ? -1 : 0);
            cout << "Direction: " << direction << endl;
            // TODO: Replace by a direction estimator based on mice input.

            // Get global data from vehicle (not accessible on real car) - also used for reference.
            Point3 positionCar = es.getPosition();
            double headingCar = es.getRotation().getAngleXY();
            if (init) {
                // This is only required to compare the estimated heading with the real vehicle's heading.
                // On the real vehicle, this value can be discared each time the algorithm restarts. The
                // only difference is that the depending internal maps will be oriented with a different
                // starting angle; but internally, they are consistent.
                INITIAL_ROTATION_CAR = headingCar;
                m_phi = INITIAL_ROTATION_CAR;
            }
            if (!init) {
                // This is only required to compare the estimated driven path with the real vehicle's driven path.
                egostateD += (positionCar - prevEgoState.getPosition()).lengthXY();
            }

            // Model in the simulation for the two mice.
            // Mounting position left mouse.
            currPositionLeftMouse = Point3(1, 0, 0);
            currPositionLeftMouse *= D/2.0;
            currPositionLeftMouse.rotateZ(headingCar + ROT_LEFT);
            currPositionLeftMouse += positionCar;

            // Mounting position right mouse.
            currPositionRightMouse = Point3(1, 0, 0);
            currPositionRightMouse *= D/2.0;
            currPositionRightMouse.rotateZ(headingCar + ROT_RIGHT);
            currPositionRightMouse += positionCar;

            // Current absolute position of the two mice (not accessible on the real car).
            cout << currPositionLeftMouse.toString() << " - " << currPositionRightMouse.toString() << endl;

            // Initialize Kalman filter.
            initializeKalmanFilter();

            // Run mice odometer algorithm from second cycle on.
            if (!init) {
                // Calculate input values as we would get them from optical mice. TODO: Put here values read from real mice.
                const double deltaXLeft = currPositionLeftMouse.getX() - prevPositionLeftMouse.getX();
                const double deltaYLeft = currPositionLeftMouse.getY() - prevPositionLeftMouse.getY();
                const double deltaXRight = currPositionRightMouse.getX() - prevPositionRightMouse.getX();
                const double deltaYRight = currPositionRightMouse.getY() - prevPositionRightMouse.getY();

                // Call algorithm for measuring and integrating driven distance and heading from two optical mice.
                calculatePosition(deltaXLeft, deltaYLeft, deltaXRight, deltaYRight, timeStep, direction);

                // Update estimated position using Kalman filter.
                estimatePosition();

                // Map headingCar into range 0..2pi (not accessible on real car).
                while (headingCar < 0) {
                    headingCar += 2 * Constants::PI;
                }
                while (headingCar > 2 * Constants::PI) {
                    headingCar -= 2 * Constants::PI;
                }

                // Calculate integration error (not accessible on real car).
                const double errorD = (egostateD - m_d);
                const double errorHeading = (headingCar - m_phi);

                Point3 calculatedPosition = Point3(m_x, m_y, 0);
                Point3 estimatedPosition = Point3(m_estimatedX, m_estimatedY, 0);

                // Print out some statistics for the algorithm's quality.
                cout << "d = " << m_d << ", phi = " << m_phi << endl;
                cout << "Actual position: " << positionCar.toString() << endl; 
                cout << "Calculated position: " << calculatedPosition.toString() << ", e: " << (positionCar - calculatedPosition).lengthXY() << endl;
                cout << "Estimated position:  " << estimatedPosition.toString() << ", e: " << (positionCar - estimatedPosition).lengthXY() << endl;
                cout << "carD = " << egostateD << ", carHeading = " << headingCar << endl;
                cout << "errorD = " << errorD << ", errorHeading = " << errorHeading << endl;
                cout << endl;

                // Add noise to position data (equally distributed between -0.001 .. 0.001.
                //m_x += ((rand() % 20) - 10)/10000.0;
                //m_y += ((rand() % 20) - 10)/10000.0;
            }

            // Save data from this cycle.
            prevTime = currentTime;
            prevEgoState = es;

            prevPositionLeftMouse = currPositionLeftMouse;
            prevPositionRightMouse = currPositionRightMouse;

            init = false;
        }

        return ModuleState::OKAY;
    }

} // miceodometer

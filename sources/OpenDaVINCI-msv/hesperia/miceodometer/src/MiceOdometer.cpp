/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <limits>

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
        m_y(0) {}

    MiceOdometer::~MiceOdometer() {}

    void MiceOdometer::setUp() {}

    void MiceOdometer::tearDown() {}

    void MiceOdometer::estimatePosition(const double &lengthLeft, const double &lengthRight, const double &timeStep, const double &direction) {
        // Algorithm for measuring and integrating driven distance and heading from two optical mice.

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

            // Calculate length of deltaYRight without being dependent on the mouse's coordinate frame.
            const double t1 = (2*X*X - lengthRight*lengthRight)/(2*X);
            const double deltaYRight = sqrt(X*X - t1*t1);

            const double PHI = asin(deltaYRight / X);
            dotD = (X + 0.5*D) * PHI * timeStep;
            dotPhi = (-1) * PHI * timeStep;
        }
        else if (lengthLeft < lengthRight) {
            // Case: turning to left.
            cout << "Case: turning to left." << endl;

            const double X = D / ( (lengthRight/lengthLeft) - 1 );

            // Calculate length of deltaYLeft without being dependent on the mouse's coordinate frame.
            const double t1 = (2*X*X - lengthLeft*lengthLeft)/(2*X);
            const double deltaYLeft = sqrt(X*X - t1*t1);

            const double PHI = asin(deltaYLeft / X);
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

        // Integrate estimated position over time and consider forwards/backwards driving.
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

        bool init = true;
        TimeStamp prevTime;
        EgoState prevEs;
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
                egostateD += (positionCar - prevEs.getPosition()).lengthXY();
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

            // Run mice odometer algorithm from second cycle on.
            if (!init) {
                const double lengthLeft = (currPositionLeftMouse - prevPositionLeftMouse).lengthXY(); // TODO: Use values from real mice.
                const double lengthRight = (currPositionRightMouse - prevPositionRightMouse).lengthXY(); // TODO: Use values from real mice.

                // Call algorithm for measuring and integrating driven distance and heading from two optical mice.
                estimatePosition(lengthLeft, lengthRight, timeStep, direction);

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

                // Print out some statistics for the algorithm's quality.
                cout << "d = " << m_d << ", phi = " << m_phi << endl;
                cout << "X = " << m_x << ", Y = " << m_y << endl;
                cout << "carD = " << egostateD << ", carHeading = " << headingCar << endl;
                cout << "errorD = " << errorD << ", errorHeading = " << errorHeading << endl;
                cout << endl;
            }

            // Save data from this cycle.
            prevTime = currentTime;
            prevEs = es;

            prevPositionLeftMouse = currPositionLeftMouse;
            prevPositionRightMouse = currPositionRightMouse;

            init = false;
        }

        return ModuleState::OKAY;
    }

} // miceodometer

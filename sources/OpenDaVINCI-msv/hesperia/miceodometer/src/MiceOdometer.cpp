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
        ConferenceClientModule(argc, argv, "MiceOdometer") {}

    MiceOdometer::~MiceOdometer() {}

    void MiceOdometer::setUp() {}

    void MiceOdometer::tearDown() {}

    ModuleState::MODULE_EXITCODE MiceOdometer::body() {
        KeyValueDataStore &kvs = getKeyValueDataStore();

        bool init = true;
        TimeStamp prevTime;
        EgoState prevEs;
        double egostateD = 0;

        // This parameter needs to be defined in the configuration: What is the initial global rotation of the car?
        double INITIAL_ROTATION_CAR = 0;

        // Distance between the two mice.
        const double D = 2;
        const double ROT_LEFT = 90.0 * Constants::DEG2RAD; // Left mouse is mounted pi/4 to the left from the vehicle's heading.
        const double ROT_RIGHT = -90.0 * Constants::DEG2RAD; // Right mouse is mounted pi/4 to the right from the vehicle's heading.

        // Data from previous cycle.
        Point3 prevPositionLeftMouse;
        Point3 prevPositionRightMouse;

        // Data from current cycle.
        Point3 currPositionLeftMouse;
        Point3 currPositionRightMouse;

        // Integral of the measured values.
        double d = 0;
        double phi = 0;

        while (getModuleState() == ModuleState::RUNNING) {
            TimeStamp currentTime;
            double timeStep = (currentTime.toMicroseconds() - prevTime.toMicroseconds()) / (1000.0 * 1000.0);

            // Get current ego state for simulating virtually mounted mice.
            Container c = kvs.get(Container::EGOSTATE);
            EgoState es = c.getData<EgoState>();

            // Get global data from vehicle (not accessible on real car) - also used for reference.
            Point3 positionCar = es.getPosition();
            double headingCar = es.getRotation().getAngleXY();
            if (init) {
                INITIAL_ROTATION_CAR = headingCar;
                phi = INITIAL_ROTATION_CAR;
            }
            if (!init) {
                egostateD += (positionCar - prevEs.getPosition()).lengthXY();
            }

            // Model in the simulation of the two mice.
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

            // Run from second cycle on.
            if (!init) {
                // Algorithm for measuring and integrating driven distance and heading from two optical mice.
                const double lengthLeft = (currPositionLeftMouse - prevPositionLeftMouse).lengthXY(); // TODO: Use real mice values here.
                const double lengthRight = (currPositionRightMouse - prevPositionRightMouse).lengthXY(); // TODO: Use real mice values here.

                cout << "lengthLeft = " << lengthLeft << ", lengthRight = " << lengthRight << endl;

                double dotD = 0;
                double dotPhi = 0;
                
                if (fabs(lengthLeft - lengthRight) < 1e-5) {
                    // Case: moving straight forward.
                    cout << "Case: moving straight forward." << endl;

                    dotD = lengthLeft * timeStep;
                    dotPhi = 0;
                }
                else if (lengthLeft > lengthRight) {
                    // Case: turning to right.
                    cout << "Case: turning to right." << endl;

                    const double X = D / ( (lengthLeft/lengthRight) - 1 );

                    // Calculate length of deltaY without being dependent on the mouse's coordinate frame.
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

                    // Calculate length of deltaY without being dependent on the mouse's coordinate frame.
                    const double t1 = (2*X*X - lengthLeft*lengthLeft)/(2*X);
                    const double deltaYLeft = sqrt(X*X - t1*t1);

                    const double PHI = asin(deltaYLeft / X);
                    dotD = (X + 0.5*D) * PHI * timeStep;
                    dotPhi = PHI * timeStep;
                }

                // Correction factor for simulation:
                dotD *= getFrequency();
                dotPhi *= getFrequency();

                // Integrate readings over time.
                d += dotD;
                phi += dotPhi;

                // Map heading into range 0..2pi.
                while (phi < 0) {
                    phi += 2 * Constants::PI;
                }
                while (phi > 2 * Constants::PI) {
                    phi -= 2 * Constants::PI;
                }
                while (headingCar < 0) {
                    headingCar += 2 * Constants::PI;
                }
                while (headingCar > 2 * Constants::PI) {
                    headingCar -= 2 * Constants::PI;
                }

                // Calculate integration error (not accessible on real car).
                const double errorD = (egostateD - d);
                const double errorHeading = (headingCar - phi);

                cout << "dotD = " << dotD << ", dotPhi = " << dotPhi << ", d = " << d << ", phi = " << phi << endl; 
                cout << "carD = " << egostateD << ", carHeading = " << headingCar << endl;
                cout << "errorD = " << errorD << ", errorHeading = " << errorHeading << endl;
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

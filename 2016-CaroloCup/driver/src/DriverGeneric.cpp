//
// Created by Mickaël on 2015-11-25.
//

#include <iostream>
#include "opendavinci/odcore/io/conference/ContainerConference.h"
#include "opendavinci/odcore/base/KeyValueConfiguration.h"

#include "DriverGeneric.h"

namespace msv {

    using namespace std;
    using namespace odcore::base;
    using namespace odcore::data;
    using namespace automotive;
    using namespace automotive::miniature;


    DriverGeneric::DriverGeneric(const int32_t &argc, char **argv) :
            TimeTriggeredConferenceClientModule(argc, argv, "Driver"),
            initialized(false),
            desiredSpeed(0.0),
            desiredSteering(0.0),
            brakeLights(false),
            flashingLightsLeft(false),
            flashingLightsRight(false),
            debug(false),
            runStartBoxSequence(true) {

        cout << "DG: arg0: " << argv[0] << endl;
        cout << "DG: arg1: " << argv[1] << endl;
        cout << "DG: arg2: " << argv[2] << endl;
    }

    DriverGeneric::~DriverGeneric() {
        if (debug)
            cout << "DriverGeneric destroyed!" << endl;
    }

    odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode DriverGeneric::body() {

        if (!initialized) {
            cout << "DriverGeneric: frequency:" << this->getFrequency() << endl;
            // Set the debug value
            KeyValueConfiguration config = getKeyValueConfiguration();
            debug = config.getValue<bool>("driver.Debug");

            Initialize();
            initialized = true;
        }

        setUp(); // Necessary since the body is called in the manager directly and not through the runModule.
        Routine();
        tearDown();

        return odcore::data::dmcp::ModuleExitCodeMessage::OKAY;
    }

    VehicleControl DriverGeneric::GetControlData() {
        // Create vehicle control data.
        VehicleControl vehicleControl;

        // With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
        vehicleControl.setSpeed(desiredSpeed);
        vehicleControl.setSteeringWheelAngle(desiredSteering);
        // You can also turn on or off various lights:
        vehicleControl.setBrakeLights(brakeLights);
        vehicleControl.setFlashingLightsLeft(flashingLightsLeft);
        vehicleControl.setFlashingLightsRight(flashingLightsRight);

        return vehicleControl;
    }

    VehicleControl DriverGeneric::GetStopControlData() {
        VehicleControl vehicleControl;
        vehicleControl.setSpeed(0);
        vehicleControl.setSteeringWheelAngle(0);
        vehicleControl.setBrakeLights(false);
        vehicleControl.setFlashingLightsLeft(false);
        vehicleControl.setFlashingLightsRight(false);
        return vehicleControl;
    }
} // msv

//
// Created by Mickaël on 2015-11-25.
//

#include "core/io/conference/ContainerConference.h"
#include "core/base/KeyValueConfiguration.h"

#include "DriverGeneric.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
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
            debug(false) { }

    DriverGeneric::~DriverGeneric() {
        if (debug)
            cout << "DriverGeneric destroyed!" << endl;
    }

// This method will do the main data processing job.
    coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode DriverGeneric::body() {

        if (!initialized) {
            // Set the debug value
            KeyValueConfiguration config = getKeyValueConfiguration();
            debug = config.getValue<bool>("driver.Debug");

            Initialize();
            initialized = true;
        }

        Routine();

        return coredata::dmcp::ModuleExitCodeMessage::OKAY;
    }

    VehicleControl DriverGeneric::GetControlData() {
        // Create vehicle control data.
        VehicleControl vc;

        // With setSpeed you can set a desired speed for the vehicle in the range of -2.0 (backwards) .. 0 (stop) .. +2.0 (forwards)
        vc.setSpeed(desiredSpeed);
        vc.setSteeringWheelAngle(desiredSteering);
        // You can also turn on or off various lights:
        vc.setBrakeLights(brakeLights);
        vc.setFlashingLightsLeft(flashingLightsLeft);
        vc.setFlashingLightsRight(flashingLightsRight);

        return vc;
    }
} // msv

//
// Created by Mickaël on 2015-11-25.
//

#include <LaneFollowingDriver.h>
#include "DriverManager.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace automotive;
    using namespace automotive::miniature;

    bool debug = true;

    DriverManager::DriverManager(const int32_t &argci, char **argvi)
            : TimeTriggeredConferenceClientModule(argci, argvi, "DriverManager"),
              argc(argci),
              argv(argvi),
              state(None),
              driver_ptr(0),
              debug(false) {
        // Deep copy of arguments
        argv = new char *[argci + 1];
        for (int i = 0; i < argci; i++) {
            int len = (int) strlen(argvi[i] + 1);
            argv[i] = new char[len];
            strcpy(argv[i], argvi[i]);
        }
        argv[argci] = NULL;
    }

    DriverManager::~DriverManager() {
        // stop car when killing driver
        VehicleControl vc;
        vc.setSpeed(0);
        Container c(Container::VEHICLECONTROL, vc);
        getConference().send(c);

        delete (driver_ptr);

        for (int i = 0; i < argc; i++) {
            delete[] argv[i];
        }
        delete[] argv;

        if (debug)
            cout << "DriverManager destroyed!" << endl;
    }

    // This method will do the main data processing job.
    coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode DriverManager::body() {

        Container proxyData;
        SensorBoardData sbd;
        bool button1; // lane following
        bool button2; // parking
        bool button3; // overtaking

        KeyValueConfiguration config = getKeyValueConfiguration();
        debug = config.getValue<bool>("driverManager.Debug");

        while (getModuleStateAndWaitForRemainingTimeInTimeslice() == coredata::dmcp::ModuleStateMessage::RUNNING) {

            // Get latest proxy data
            proxyData = getKeyValueDataStore().get(Container::USER_DATA_0);
            sbd = proxyData.getData<SensorBoardData>();

            // Get buttons data from proxy
            button1 = (bool) (int) sbd.getValueForKey_MapOfDistances(9);
            button2 = (bool) (int) sbd.getValueForKey_MapOfDistances(10);
            button3 = (bool) (int) sbd.getValueForKey_MapOfDistances(11);

            button1 = true;

            if (debug) {
                cout << endl << "DriverManager: " << flush;
                cout << "Button1:" << button1 << ", Button2:" << button2 << ", Button3:" << button3 << flush;
                cout << ", state:" << state << endl;
            }

            // Check if current buttons correspond to current state and driver
            if (button1 && !button2 && !button3) {
                if (state != Lane_Following) {
                    if (debug)
                        cout << "Creation Lane Following driver" << endl;
                    driver_ptr = new LaneFollowingDriver(argc, argv);
                    if (!driver_ptr) {
                        if (debug)
                            cout << "Memory error" << endl;
                        continue; // TODO Improve error management
                    }
                    driver_ptr->runModule(); // Necessary to run it once to initialize the module entirely
                    state = Lane_Following;
                }
            }
            else if (!button1 && button2 && !button3) {
                if (state != Parking) {
                    //driver_ptr = new ParkingDriver(argc, argv);
                    if (!driver_ptr) {
                        if (debug)
                            cout << "Memory error" << endl;
                        continue; // TODO Improve error management
                    }
                    driver_ptr->runModule(); // Necessary to run it once to initialize the module entirely
                    state = Parking;
                }
            }
            else if (!button1 && !button2 && button3) {
                if (state != Overtaking) {
                    //driver_ptr = new OvertakingDriver(argc, argv);
                    if (!driver_ptr) {
                        if (debug)
                            cout << "Memory error" << endl;
                        continue; // TODO Improve error management
                    }
                    driver_ptr->runModule(); // Necessary to run it once to initialize the module entirely
                    state = Overtaking;
                }
            }
            else {
                driver_ptr = 0;
                state = None;
                // Create default control value for car waiting
                VehicleControl vc;
                vc.setSpeed(0);
                vc.setSteeringWheelAngle(0);
                vc.setBrakeLights(false);
                vc.setFlashingLightsLeft(false);
                vc.setFlashingLightsRight(false);
                Container c(Container::VEHICLECONTROL, vc);
                // Send container.
                getConference().send(c);
            }

            // Call driver's body and send resulting vehicle control
            if (driver_ptr != 0) {
                driver_ptr->body();
                // Create container for finally sending the data.
                Container c(Container::VEHICLECONTROL, driver_ptr->GetControlData());
                // Send container.
                getConference().send(c);
            }
        }

        return coredata::dmcp::ModuleExitCodeMessage::OKAY;
    }


} // msv

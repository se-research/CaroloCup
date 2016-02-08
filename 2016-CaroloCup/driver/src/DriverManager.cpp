//
// Created by Mickaël Fourgeaud on 2015-11-25.
// Email: mfourgeaud@gmail.com
//

#include <core/base/LIFOQueue.h>
#include <LaneFollowingDriver.h>
#include <ParkingDriver.h>
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
        state = None;
    }

    DriverManager::~DriverManager() {

        if (debug)
            cout << "Destroying DriverManager!" << endl;

        // stop car when killing driver
        stopCar();

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

        core::base::LIFOQueue lifo;
        addDataStoreFor(Container::USER_DATA_0, lifo);

        KeyValueConfiguration config = getKeyValueConfiguration();
        debug = config.getValue<bool>("driverManager.Debug");

        if (debug)
            cout << endl << "DriverManager debug on. " << flush;

        while (getModuleStateAndWaitForRemainingTimeInTimeslice() == coredata::dmcp::ModuleStateMessage::RUNNING) {

            // Get latest proxy data
            proxyData = lifo.pop();
            sbd = proxyData.getData<SensorBoardData>();

            // Get buttons data from proxy
            button1 = (bool) (int) sbd.getValueForKey_MapOfDistances(9);
            button2 = (bool) (int) sbd.getValueForKey_MapOfDistances(10);
            button3 = (bool) (int) sbd.getValueForKey_MapOfDistances(11);

            // clear lifo to avoid filling memory
            lifo.clear();

            if (debug) {
                cout << "Button1:" << button1 << ", Button2:" << button2 << ", Button3:" << button3 << flush;
                cout << ", state:" << state << endl;
            }

            // Check if current buttons correspond to current state and driver
            if (button1 && !button2 && !button3) {
                if (state != Lane_Following) {
                    if (debug)
                        cout << "Creating Lane Following driver" << endl;
                    state = Lane_Following;
                    driver_ptr = new LaneFollowingDriver(argc, argv);
                    if (!driver_ptr) {
                        state = None;
                        if (debug)
                            cout << "DriverManager: Memory error when creating driver." << endl;
                        continue; // TODO Improve error management
                    }
                    driver_ptr->runModule(); // Necessary to run it once to initialize the module entirely
                }
            }
            else if (!button1 && button2 && !button3) {
                if (state != Parking) {
                    if (debug)
                        cout << "Creating Parking driver" << endl;
                    driver_ptr = new ParkingDriver(argc, argv);
                    if (!driver_ptr) {
                        state = None;
                        if (debug)
                            cout << "DriverManager: Memory error when creating driver." << endl;
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
                        state = None;
                        if (debug)
                            cout << "DriverManager: Memory error when creating driver." << endl;
                        continue; // TODO Improve error management
                    }
                    driver_ptr->runModule(); // Necessary to run it once to initialize the module entirely
                    state = Overtaking;
                }
            }
            else {
                cout << "DriverManager: Sending default state." << endl;
                state = None;
                stopCar();
                // TODO safe delete of pointer when switching state only
                //if (driver_ptr)
                //{
                //   delete driver_ptr;
                //}
            }

            // Call driver's body and send resulting vehicle control
            if (state != None) {
                driver_ptr->body();
                // Create container for finally sending the data.
                Container c(Container::VEHICLECONTROL, driver_ptr->GetControlData());
                // Send container.
                getConference().send(c);
            }
        }

        return coredata::dmcp::ModuleExitCodeMessage::OKAY;
    }

    void DriverManager::stopCar() {
        Container c(Container::VEHICLECONTROL, DriverGeneric::GetStopControlData());
        getConference().send(c);
    }

} // msv

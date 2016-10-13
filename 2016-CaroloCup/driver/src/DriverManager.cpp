//
// Created by Mickaël on 2015-11-25.
//

#include <opendavinci/odcore/base/LIFOQueue.h>
#include <LaneFollowingDriver.h>
#include <ParkingDriver.h>
#include "DriverManager.h"

namespace msv {

    using namespace std;
    using namespace odcore::base;
    using namespace odcore::data;
    using namespace automotive;
    using namespace automotive::miniature;

    bool debug = true;

    DriverManager::DriverManager(const int32_t &argci, char **argvi)
            : TimeTriggeredConferenceClientModule(argci, argvi, "DriverManager"),
              argc(argci),
              argv(argvi),
              driver_state(Neutral),
              driverGeneric(0),
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
        stopCar();

        delete (driverGeneric);

        for (int i = 0; i < argc; i++) {
            delete[] argv[i];
        }
        delete[] argv;

        if (debug)
            cout << "DriverManager destroyed!" << endl;
    }

    // This method will do the main data processing job.
    odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode DriverManager::body() {

        Container proxyDataContainer;
        SensorBoardData sensorBoardData;
        bool laneFollowingButton; // lane following
        bool parkingButton; // parking
        bool overtakingButton; // overtaking

        KeyValueConfiguration config = getKeyValueConfiguration();
        debug = config.getValue<bool>("driverManager.Debug");

        if (debug)
            cout << endl << "DriverManager: " << flush;

        odcore::base::LIFOQueue lifo;
        addDataStoreFor(SensorBoardData::ID(), lifo);

        while (getModuleStateAndWaitForRemainingTimeInTimeslice() == odcore::data::dmcp::ModuleStateMessage::RUNNING) {

            // Get latest proxy data
            proxyDataContainer = lifo.pop();
            sensorBoardData = proxyDataContainer.getData<SensorBoardData>();

            // Get buttons data from proxy
            laneFollowingButton = (bool) (int) sensorBoardData.getValueForKey_MapOfDistances(9);
            parkingButton = (bool) (int) sensorBoardData.getValueForKey_MapOfDistances(10);
            overtakingButton = (bool) (int) sensorBoardData.getValueForKey_MapOfDistances(11);

            // clear lifo to avoid filling memory
            lifo.clear();

            if (debug) {
                cout << "LaneFollowingButton:" << laneFollowingButton << ", ParkingButton:" << parkingButton << ", Overtaking button:" << overtakingButton << flush;
                cout << ", driver state:" << driver_state << endl;
            }

            // Check if current buttons correspond to current state and driver
            if (laneFollowingButton && !parkingButton && !overtakingButton) {
                if (driver_state != Lane_Following) {
                    if (debug)
                        cout << "Creating Lane Following driver" << endl;
                    driverGeneric = new LaneFollowingDriver(argc, argv);
                    if (!driverGeneric) {
                        if (debug)
                            cout << "Memory error" << endl;
                        continue; // TODO Improve error management
                    }
                    driverGeneric->runModule(); // Necessary to run it once to initialize the module entirely
                    driver_state = Lane_Following;
                }
            }
            else if (!laneFollowingButton && parkingButton && !overtakingButton) {
                if (driver_state != Parking) {
                    if (debug)
                        cout << "Creating Parking driver" << endl;
                    driverGeneric = new ParkingDriver(argc, argv);
                    if (!driverGeneric) {
                        if (debug)
                            cout << "Memory error" << endl;
                        continue; // TODO Improve error management
                    }
                    driverGeneric->runModule(); // Necessary to run it once to initialize the module entirely
                    driver_state = Parking;
                }
            }
            else if (!laneFollowingButton && !parkingButton && overtakingButton) {
                if (driver_state != Overtaking) {
                    //driverGeneric = new OvertakingDriver(argc, argv);
                    if (!driverGeneric) {
                        if (debug)
                            cout << "Memory error" << endl;
                        continue; // TODO Improve error management
                    }
                    driverGeneric->runModule(); // Necessary to run it once to initialize the module entirely
                    driver_state = Overtaking;
                }
            }
            else {
                driverGeneric = 0;
                driver_state = Neutral;
                stopCar();
            }

            // Call driver's body and send resulting vehicle control
            if (driverGeneric != 0) {
                driverGeneric->body();
                // Create container for finally sending the data.
                Container container(driverGeneric->GetControlData());
                // Send container.
                getConference().send(container);
            }
        }

        return odcore::data::dmcp::ModuleExitCodeMessage::OKAY;
    }

    void DriverManager::stopCar() {
        Container container(DriverGeneric::GetStopControlData());
        getConference().send(container);
    }

} // msv

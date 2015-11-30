//
// Created by li on 2015-11-29.
//



#include "ParkingDriver.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace automotive;
    using namespace automotive::miniature;

    ParkingDriver::ParkingDriver(const int32_t &argc, char **argv) :

            DriverGeneric(argc, argv),
            parking_state(NONE) { }


    ParkingDriver::~ParkingDriver() { }

    void ParkingDriver::Routine() {
        Container conUserData1 = getKeyValueDataStore().get(Container::USER_DATA_1);
        if(parking_state == SEARCHSPOT){
            // first stage
        }
        else if(parking_state == FOUNDSPOT){
            // find possible parkingspot and stoped
        }
        else if(parking_state == BACKRIGHT){
            // overide helper driver
        }
        else if(parking_state == BACKLEFT ){
            // adjust trajectory
        }
        else if(parking_state == ADJUST){
            // moving forwards for adjust
        }
        else if(parking_state == FINISH){
            // finish parking blinking
        }
        else if(parking_state == NONE){
            // err state
        }


    }

    void ParkingDriver::Initialize() {
        if (debug)
            cout << "Parking Init" << endl;
    }


}
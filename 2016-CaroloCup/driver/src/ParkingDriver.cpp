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
            DriverGeneric(argc, argv){ }


    ParkingDriver::~ParkingDriver() { }

    void ParkingDriver::Routine() {


    }

    void ParkingDriver::Initialize() {
        if (debug)
            cout << "Parking Init" << endl;
    }


}
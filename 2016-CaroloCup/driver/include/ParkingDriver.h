//
// Created by Mickaël on 2015-12-11.
//

#ifndef AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H
#define AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H

#include <DriverGeneric.h>

namespace msv {

    using namespace std;

    enum DRIVING_STATE {
        DRIVE = 0,
        START_OBST = 1,
        POSSIBLE_SPOT = 2,
        STOP_FOR_PARKING = 3,
        PARKING = 4,
        NO_POSSIBLE_PARKING_PLACE = 5

    };

    enum PARKING_STATE {
        BACKWARDS_RIGHT = 0,
        BACKWARDS_LEFT = 1,
        FORWARD_RIGHT = 2,
        BACK_AGAIN = 3,
        STOP = 4,
        DONE = 5
    };

    class ParkingDriver : public DriverGeneric {


    public:

        ParkingDriver(const int32_t &argc, char **argv);

        ~ParkingDriver();

        void Routine();

        void Initialize();

    private:

        DRIVING_STATE driving_state;
        PARKING_STATE parking_state;

        void setUp() { };

        void tearDown() { };

        void parking();

    };


}


#endif //AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H

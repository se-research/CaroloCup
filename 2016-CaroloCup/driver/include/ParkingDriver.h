//
// Created by Mickaël on 2015-12-11.
//

#ifndef AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H
#define AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H

#include <DriverGeneric.h>

namespace msv {

    using namespace std;

    enum DRIVING_STATE {
        DRIVE,
        START_OBST,
        POSSIBLE_SPOT,
        STOP_FOR_PARKING,
        INIT_PARKING,
        PARKING,
        NO_POSSIBLE_PARKING_PLACE
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

        // Not implemented
        ParkingDriver(const ParkingDriver &);

        ParkingDriver &operator=(const ParkingDriver &driver);

    private:

        DRIVING_STATE driving_state;
        PARKING_STATE parking_state;
        LaneFollowingDriver *laneDriver;

        void setUp() { };

        void tearDown() { };

        void parking();

	int32_t m_timestamp;

	float previousError ;
        float SpeedControl(float setSpeed,float actualSpeed,double timeStep, float *); 

    };


}


#endif //AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H

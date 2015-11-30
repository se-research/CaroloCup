//
// Created by li on 2015-11-29.
//

#ifndef AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H
#define AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H


#include <DriverGeneric.h>
#include "LaneDetectionData.h"
#include "opencv2/opencv.hpp"

namespace msv {

    using namespace std;
    using namespace cv;

    enum PARKING_STATE {
        SEARCHSPOT=1,
        FOUNDSPOT =2,
        BACKRIGHT =3,
        BACKLEFT  =4,
        ADJUST    =5,
        FINISH    =6,
        NONE = 99
    };

    class ParkingDriver : public DriverGeneric {

    public:

        ParkingDriver(const int32_t &argc, char **argv);

        ~ParkingDriver();

        void Routine();

        void Initialize();

    private:
        PARKING_STATE parking_state ;

        void setUp() { };

        void tearDown() { };

        // Define control parameters

    };

} // msv


#endif //AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H

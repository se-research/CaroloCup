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

    class ParkingDriver : public DriverGeneric {

    public:

        ParkingDriver(const int32_t &argc, char **argv);

        ~ParkingDriver();

        void Routine();

        void Initialize();

    private:

        void setUp() { };

        void tearDown() { };

        // Define control parameters

    };

} // msv


#endif //AUTOMOTIVE_CAROLOCUP_PARKINGDRIVER_H

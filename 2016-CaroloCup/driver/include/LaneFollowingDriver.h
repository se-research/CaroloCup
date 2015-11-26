//
// Created by ubuntu on 2015-11-25.
//

#ifndef AUTOMOTIVE_CAROLOCUP_LANEFOLLOWINGDRIVER_H
#define AUTOMOTIVE_CAROLOCUP_LANEFOLLOWINGDRIVER_H

#include <DriverGeneric.h>
#include "LaneDetectionData.h"
#include "opencv2/opencv.hpp"

namespace msv {

    using namespace std;
    using namespace cv;

    class LaneFollowingDriver : public DriverGeneric {

    public:

        LaneFollowingDriver(const int32_t &argc, char **argv);

        ~LaneFollowingDriver();

        void Routine();

        void Initialize();

    private:

        void setUp() { };

        void tearDown() { };

        float calculateDesiredHeading(float oldLateralError);

        void calculateErr(CustomLine, CustomLine, float *, double *);

        bool laneFollowing(LaneDetectionData *data);

        bool m_hasReceivedLaneDetectionData;
        bool after_intersection;

        // Define control parameters
        float m_angularError;
        float m_speed;
        double m_lateralError;
        double m_intLateralError;
        double m_derLateralError;
        float m_desiredSteeringWheelAngle;
        float m_propGain;
        float m_intGain;
        float m_derGain;
        double SCALE_FACTOR;    //For example, 12000 dpm (dots-per-meter)

        int32_t m_timestamp;

        Vec4i m_leftLine;
        Vec4i m_rightLine;
        Vec4i m_dashedLine;
    };

} // msv


#endif //AUTOMOTIVE_CAROLOCUP_LANEFOLLOWINGDRIVER_H

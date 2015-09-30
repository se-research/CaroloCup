//
// Created by parallels on 9/30/15.
//

#ifndef AUTOMOTIVE_CAROLOCUP_LINEDETECTOR_H
#define AUTOMOTIVE_CAROLOCUP_LINEDETECTOR_H

#include "opencv2/imgproc/imgproc.hpp"

using namespace cv;

struct Config
{
    int th1, th2, hlTh, caThVal, caThMax, caThTyp, pGain, intGain, derGain,
            houghMinAngle, houghMaxAngle, houghStartVal, houghMaxLines,
            XTimesYMin, XTimesYMax, maxY, maxArea;
};

class LineDetector {
private:
    vector<Vec4i> hierarchy;

public:
    LineDetector(Config m_config);
    void toGrayScale();


};


#endif //AUTOMOTIVE_CAROLOCUP_LINEDETECTOR_H

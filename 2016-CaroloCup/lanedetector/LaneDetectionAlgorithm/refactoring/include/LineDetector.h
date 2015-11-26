//
// Created by Casper CY Chiang on 10/8/15.
//

#ifndef AUTOMOTIVE_CAROLOCUP_LINEDETECTOR_H
#define AUTOMOTIVE_CAROLOCUP_LINEDETECTOR_H

#include <iostream>
#include <opencv2/imgproc/imgproc.hpp>

using namespace std;
using namespace cv;

class LineDetector {
private:
    Mat image;
    void getContours();

public:
    LineDetector();
    void processImage(Mat &input);
//    void print();
};


#endif //AUTOMOTIVE_CAROLOCUP_LINEDETECTOR_H

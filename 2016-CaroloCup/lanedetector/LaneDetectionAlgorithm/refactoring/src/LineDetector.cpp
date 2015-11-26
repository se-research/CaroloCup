//
// Created by Casper CY Chiang on 10/8/15.
//

#include "LineDetector.h"

LineDetector::LineDetector() {

}
void LineDetector::processImage(Mat &input) {
    cout << "Hello World." << endl;
}
void LineDetector::getContours() {
    findContours(image, contours, hierarchy, CV_RETR_TREE,
                 CV_CHAIN_APPROX_SIMPLE, Point(0, 0));
}
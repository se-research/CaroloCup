//
// Created by parallels on 9/30/15.
//

#include "LineDetector.h"

LineDetector::LineDetector(Config m_config) {
    m_config.XTimesYMin = 2;
    m_config.XTimesYMax = 20;
    m_config.maxY = 235;
    m_config.maxArea = 4;
    m_config.debug = false;

}

void LineDetector::getContours() {
//    if(m_config.debug) {
//        startTimer = chrono::high_resolution_clock::now();
//    }
    findContours(image, contours, hierarchy, CV_RETR_TREE,
                 CV_CHAIN_APPROX_SIMPLE, Point(0, 0));
//    if(m_config.debug) {
//        startTimer = chrono::high_resolution_clock::now();
//    }
}

void LineDetector::getPolygonContours() {
    contours_poly.resize(contours.size());
    dashLines = solidLines = vector<CustomLine>(contours.size());
    for (unsigned int i = 0; i < contours.size(); i++) {
        approxPolyDP(Mat(contours[i]), contours_poly[i], 3, true);
    }
}

void LineDetector::getBoundingBoxes() {
    // RotatedRect declare in the LineDetector.h

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        rect = minAreaRect(contours_poly[i]);
        Point2f rect_points[4];
        rect.points(rect_points);
        for (int j = 0; j < 4; j++) {
//            naming problem
//            int sizeX = 0, sizeY = 0, sizeR = 0;
//            sizeX = distance
        }
    }
}

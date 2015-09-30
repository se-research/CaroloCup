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
    bool debug;
};


class CustomLine {
public:
    CustomLine() :
            p1(),
            p2(),
            slope(0),
            polygonIndex(-1) // This is a mapping to the polygon that the CustomLine was derived from.
    { }

    virtual ~CustomLine() { }

    bool operator<(const CustomLine &other) const {
        return max(p1.y, p2.y) > max(other.p1.y, other.p2.y);
        //return slope < other.slope;
    }

    bool operator==(const CustomLine &other) const {
        if ((p1.y == other.p1.y) && (p1.x == other.p1.x)) {
            return true;
        }
        return false;
    }

    Point p1, p2;
    float slope;
    int polygonIndex;
};

class LineDetector {
private:
    Mat image;

    Config m_config;
    vector<Vec4i> hierarchy;
    vector<vector<Point> > contours;
    vector<vector<Point> > contours_poly;
    vector<CustomLine> dashLines;
    vector<CustomLine> solidLines;
    RotatedRect rect;
    void getContours();
    void getPolygonContours();
    void getBoundingBoxes();

public:
    LineDetector(Config m_config);



};


#endif //AUTOMOTIVE_CAROLOCUP_LINEDETECTOR_H

#ifndef LANEDETECTIONALGORITHM_MAIN_H
#define LANEDETECTIONALGORITHM_MAIN_H

#endif //LANEDETECTIONALGORITHM_MAIN_H

#include <iostream>
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"
/// example to show how to use measure_time.h
#include "measure_time.h"
//#include "/usr/include/opendavinci/core/data/TimeStamp.h"

//using namespace core::data;

#define MIN_ANGLE 15
using namespace std;
using namespace cv;

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

struct PolySize {
    int sizeX, sizeY, sizeR;
    Point shortSideMiddle;
    Point longSideMiddle;
};

struct Config
{
    int th1, th2, hlTh, caThVal, caThMax, caThTyp, pGain, intGain, derGain,
            houghMinAngle, houghMaxAngle, houghStartVal, houghMaxLines,
            XTimesYMin, XTimesYMax, maxY, maxArea;
};

class Lines
{
public:
    Lines () :
            leftLine(Vec4i(0, 0, 0, 0)) ,
            rightLine(Vec4i(0, 0, 0, 0)) ,
            dashedLine(Vec4i(0, 0, 0, 0)),
            goalLine(),
            goalLineLeft(),
            currentLine(),
            pGain(0),
            intGain(0),
            derGain(0),
            speed(0),
            width(0),
            height(0),
            startLineHeight(0),
            stopLineHeight(0)
    {}
    Lines (Vec4i l, Vec4i d, Vec4i r) :
            leftLine(l) ,
            rightLine(r) ,
            dashedLine(d),
            goalLine(),
            goalLineLeft(),
            currentLine(),
            pGain(0),
            intGain(0),
            derGain(0),
            speed(0),
            width(0),
            height(0),
            startLineHeight(0),
            stopLineHeight(0)
    {}
    virtual ~Lines () {}
    void setGoalLine(const CustomLine &goal)
    {
        goalLine = goal;
    }
    void setGoalLineLeft(const CustomLine &goal)
    {
        goalLineLeft = goal;
    }
    void setCurrentLine(const CustomLine &curr)
    {
        currentLine = curr;
    }



    Vec4i leftLine;
    Vec4i rightLine;
    Vec4i dashedLine;
    CustomLine goalLine;
    CustomLine goalLineLeft;
    CustomLine currentLine;
    int pGain;
    int intGain;
    int derGain;
    int speed;
    int width;
    int height;
    int startLineHeight;
    int stopLineHeight;


};

struct LinesToUse
{
    CustomLine dashLine;
    CustomLine rightLine;
    CustomLine leftLine;
    vector<CustomLine> dashedCurve; // debug
    int cntDash;    // debug
    bool dashedCurveFound;
    bool foundD;
    bool foundR;
    bool foundL;
    bool foundGoal;
    Vec4i dashLineVec;
    Vec4i leftLineVec;
    Vec4i rightLineVec;
    bool isLeftEstimated;
    bool isDashEstimated;
    bool isRightEstimated;
    Lines *lines;
};

enum RoadState{
    NOT_SET,
    NORMAL,
    INTERSECTION
};


LinesToUse ltu;

Mat image;
Mat originalImage;
int previousThresh = 48;
vector<vector<Point> > contours_poly;
vector<vector<Point> > contours;
vector<CustomLine> dashLines;
vector<PolySize> line_sizes;
vector<RotatedRect> rects;
vector<CustomLine> solidLines;
float minXI, minYI, YI;

Config m_config;

int cntDash = 0;
int cntSolid = 0;
int h, w, offset;
bool foundStopStartLine = false;

RoadState roadState = NORMAL;

int currentDashGoalX = 0;
int currentRightGoalX = 0;
int currentLeftGoalX = 0;
int calcRoadSize, calcRoadAngle;

RotatedRect bigRect;
bool intersectionOn = false;
bool foundIntersection = false;
int intersectionRect;
bool calcIntersectionGoalLine = false;
long intersection_start;


int readImage(char *imageName, int argc);

void toGrayScale();

void cropImage();

int getDynamicThresh(int lux);

void applyThreshold();

void getAndDisplayContours();

void getAndDisplayPolygonContours();

void getAndDisplayRectangles();

void getDashedLines();

float getLineSlope(Point &p1, Point &p2);

CustomLine createLineFromRect(RotatedRect *rect, int sizeX, int sizeY, int polygonIndex);

void displayDashedLines();

void classifyLines();

void displaySolidLines();

void filterAndMerge();

void displayBothLineTypes();

void finalFilter();

void characteristicFiltering(LinesToUse *ltu);

int getIntersectionWithBottom(CustomLine l);

vector<CustomLine> findCurve(vector<CustomLine> lines);

float getDist(const Point p1, const Point p2);
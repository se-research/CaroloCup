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
#define MIN_ANGLE 15
#define LEFT_ROAD_SIZE 350
#define ROAD_SIZE 685//770
#define ROAD_GOAL 0.5
#define ROAD_ANGLE 85
#define MID_DASH_ANGLE -47
#define CONFIDENCE_LEVEL_MAX 5
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

struct EstimationData {
    bool isLeftEstimated;
    bool isDashEstimated;
    bool isRightEstimated;
    bool foundGoal;
    CustomLine left;
    CustomLine dash;
    CustomLine right;
    int calcRoadSize;
    int yPosition;
};
struct GoalLineData {
    CustomLine rightGoalLine;
    CustomLine leftGoalLine;
    int confidenceLevel_rightGoalLine;
};
class LaneDetectorDataToDriver
{
public:
    LaneDetectorDataToDriver () :
            switchPointsLeftGoalLines0(),
            switchPointsRightGoalLines0(),
            switchPointsLeftGoalLines1(),
            switchPointsRightGoalLines1(),
            leftGoalLines0(),
            rightGoalLines0(),
            leftGoalLines1(),
            rightGoalLines1(),
            leftGoalLines2(),
            rightGoalLines2(),
            leftGoalLines3(),
            rightGoalLines3(),
            currentLine(),
            roadState(NORMAL),
            confidenceLevel(0),
            confidenceLevel_goalLine(0),
            noTrajectory(true)
    {}
    LaneDetectorDataToDriver (vector<int> spl, vector<int> spr, vector<CustomLine> lgl, vector<CustomLine> rgl, CustomLine c, bool nothing, int clgl):
            switchPointsLeftGoalLines0(spl[0]),
            switchPointsRightGoalLines0(spr[0]),
            switchPointsLeftGoalLines1(spl[1]),
            switchPointsRightGoalLines1(spr[1]),
            leftGoalLines0(lgl[0]),
            rightGoalLines0(rgl[0]),
            leftGoalLines1(lgl[1]),
            rightGoalLines1(rgl[1]),
            leftGoalLines2(lgl[2]),
            rightGoalLines2(rgl[2]),
            leftGoalLines3(lgl[3]),
            rightGoalLines3(rgl[3]),
            currentLine(c),
            roadState(NORMAL),
            confidenceLevel(0),
            confidenceLevel_goalLine(clgl),
            noTrajectory(nothing)
    {}
    LaneDetectorDataToDriver (CustomLine lgl, CustomLine rgl, CustomLine c, bool nothing, int clgl):
            switchPointsLeftGoalLines0(),
            switchPointsRightGoalLines0(),
            switchPointsLeftGoalLines1(),
            switchPointsRightGoalLines1(),
            leftGoalLines0(lgl),
            rightGoalLines0(rgl),
            leftGoalLines1(),
            rightGoalLines1(),
            leftGoalLines2(),
            rightGoalLines2(),
            leftGoalLines3(),
            rightGoalLines3(),
            currentLine(c),
            roadState(NORMAL),
            confidenceLevel(0),
            confidenceLevel_goalLine(clgl),
            noTrajectory(nothing)
    {}
    virtual ~LaneDetectorDataToDriver () {}

    void setRoadState(RoadState state){
        roadState=state;
    }
    void setConfidence(int conf){
        confidenceLevel = conf;
    }
    int switchPointsLeftGoalLines0;
    int switchPointsRightGoalLines0;
    int switchPointsLeftGoalLines1;
    int switchPointsRightGoalLines1;
    CustomLine leftGoalLines0;
    CustomLine rightGoalLines0;
    CustomLine leftGoalLines1;
    CustomLine rightGoalLines1;
    CustomLine leftGoalLines2;
    CustomLine rightGoalLines2;
    CustomLine leftGoalLines3;
    CustomLine rightGoalLines3;
    CustomLine currentLine;
    RoadState roadState;
    int confidenceLevel;
    int confidenceLevel_goalLine;
    bool noTrajectory;
};

LaneDetectorDataToDriver *dataToDriver;
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

vector<Vec4i> hierarchy;

Config m_config;

int cntDash = 0;
int cntSolid = 0;
int h, w, offset;
bool foundStopStartLine = false;

int currentDashGoalX = 0;
int currentRightGoalX = 0;
int currentLeftGoalX = 0;
int calcRoadSize, calcRoadAngle;

RotatedRect bigRect;
bool intersectionOn = false;
bool foundIntersection = false;
RoadState roadState = NORMAL;
int intersectionRect;
bool calcIntersectionGoalLine = false;
auto intersection_start = chrono::high_resolution_clock::now();

int readImage(char *imageName, int argc);

void toGrayScale();

void cropImage();

int getDynamicThresh(int lux);

void applyThreshold();

void getContours();

void getPolygonContours();

void getBoundingBoxes();

float getLineSlope(Point &p1, Point &p2);

CustomLine createLineFromRect(RotatedRect *rect, int sizeX, int sizeY, int polygonIndex);

void classifyLines();

void displayDashedLines();

void displaySolidLines();

void displayBothLines(string Title);

void filterAndMerge();

void finalFilter();

void characteristicFiltering(LinesToUse *ltu);

int getIntersectionWithBottom(CustomLine l);

vector<CustomLine> findCurve(vector<CustomLine> lines);

float getDist(const Point p1, const Point p2);

void displaySelectedLines();

CustomLine getNoneCustomLine();

int getRoadAngle(int lineDetected, int lineAngle);

int getRoadSize(int roadAngleVal);

CustomLine simple_calculateGoalLine(CustomLine fst, CustomLine snd, EstimationData *ed);

int getIntersectionWithY(CustomLine l, int y);

void provideGoalLine(EstimationData *ed, GoalLineData *gld);

bool isNoneCustomLine(CustomLine aspirant);

void provideGoalLine(EstimationData *ed, GoalLineData *gld);

int getIntersectionWithTop(CustomLine l);

void createTrajectory(LinesToUse *ltu);

void displayTrajectory();

bool getVanishingPoint(Point2f o1, Point2f p1, Point2f o2, Point2f p2,
                       Point2f &r);

int getIntersectionWithTopP2(CustomLine l);

PolySize createPolySize(const RotatedRect &rect);

void displayContours();

void displayPolygonContours();

void displayBoundingBoxes();

void createIntersectionGoalLine();

void displayIntersection() ;
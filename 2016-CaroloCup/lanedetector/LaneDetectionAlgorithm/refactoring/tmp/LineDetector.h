//
// Created by parallels on 9/30/15.
//

#include "opencv2/imgproc/imgproc.hpp"
#include <iostream>
#include <chrono>

using namespace cv;
using namespace std;

#define MIN_ANGLE 15
#define MIN_ANGLE 15
#define LEFT_ROAD_SIZE 350
#define ROAD_SIZE 685//770
#define ROAD_GOAL 0.5
#define ROAD_ANGLE 85
#define MID_DASH_ANGLE -47
#define CONFIDENCE_LEVEL_MAX 5
auto intersection_start = chrono::high_resolution_clock::now();

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

struct PolySize {
    int sizeX, sizeY, sizeR;
    Point shortSideMiddle;
    Point longSideMiddle;
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
//    Lines *lines;
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

class LineDetector {
private:
    Mat image;
    int previousThresh = 48;
    /// Counter for Solid line.
    int cntSolid;
    int cntDash;
    Config m_config;
    vector<Vec4i> hierarchy;
    vector<vector<Point> > contours;
    vector<vector<Point> > contours_poly;
    vector<CustomLine> dashLines;
    vector<CustomLine> solidLines;
    vector<RotatedRect> rects;
    vector<PolySize> line_sizes;
    RotatedRect rect;
    RotatedRect bigRect;
    RoadState roadState;
    int calcRoadSize;
    int calcRoadAngle;

    void getContours();
    void getPolygonContours();
    void getBoundingBoxes();
    void classifyLines();
    void filterAndMerge();
    void finalFilter();
    void characteristicFiltering(LinesToUse *ltu);
    void createTrajectory(LinesToUse *ltu);
    void createIntersectionGoalLine();

    //Helper functions
    float getLineSlope(Point &p1, Point &p2);
    Point getLineCenter(Point &p1, Point &p2); //NOT SURE ABOUT '&'
    float getDist(const Point p1, const Point p2);
    CustomLine createLineFromRect(RotatedRect *rect, int sizeX, int sizeY, int polygonIndex);
    int getDynamicThresh(int lux);
    vector<CustomLine> findCurve(vector<CustomLine> lines);
    bool isNoneCustomLine(CustomLine aspirant);
    CustomLine getNoneCustomLine();
    void provideGoalLine(EstimationData *ed, GoalLineData *gld);
    int getRoadAngle(int lineDetected, int lineAngle);
    int getRoadSize(int roadAngleVal);
    CustomLine simple_calculateGoalLine(CustomLine fst, CustomLine snd, EstimationData *ed);
    int getIntersectionWithY(CustomLine l, int y);
    int getIntersectionWithTop(CustomLine l);
    int getIntersectionWithTopP2(CustomLine l);
    int getIntersectionWithBottom(CustomLine l);
    PolySize createPolySize(const RotatedRect &rect);


    float minXI, minYI, YI;
    int h, w, offset;
    int intersectionRect;
    bool intersectionOn = false;
    bool foundIntersection = false;
    bool calcIntersectionGoalLine = false;
    bool foundStopStartLine = false;




public:
    LineDetector();
    void processImage(Mat input);

};

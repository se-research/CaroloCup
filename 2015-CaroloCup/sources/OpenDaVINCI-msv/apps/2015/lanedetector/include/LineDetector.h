#ifndef LINEDETECTOR_H_
#define LINEDETECTOR_H_

#define MIN_ANGLE 15
#define ROAD_SIZE 650//770
#define ROAD_GOAL 0.5
#define ROAD_ANGLE 85
#define MID_DASH_ANGLE -47

#include <queue>
#include "opencv2/opencv.hpp"
#include "LineDetectorTypes.h"
#include "LaneDetectionData.h"
#include <numeric>

using namespace cv;
using namespace std;

namespace msv
{

struct Config
{
    int th1, th2, hlTh, caThVal, caThMax, caThTyp, pGain, intGain, derGain,
        houghMinAngle, houghMaxAngle, houghStartVal, houghMaxLines,
        XTimesYMin, XTimesYMax, maxY, maxArea;
};

struct PolySize
{
    int sizeX, sizeY, sizeR;
    Point shortSideMiddle;
    Point longSideMiddle;
};

// Structs for intermediate results
struct IntermediateResult_getContours
{
    vector<vector<Point> > contours;
};

struct IntermediateResult_getRectangles
{
    vector<RotatedRect> rects;
};

struct IntermediateResult
{
    vector<CustomLine> dashLines;
    vector<CustomLine> solidLines;
    int cntDash;
    int cntSolid;
    bool foundStopStartLine;
    bool intersectionOn;
    bool foundIntersection;
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

// Container for information to give to the driver from LaneDetection
struct FinalOutput {
	vector<int> cutPoints;
    vector<CustomLine> left;
    vector<CustomLine> dash;
    vector<CustomLine> right;
    vector<bool> estimatedLeft;
    vector<bool> estimatedDash;
    vector<bool> estimatedRight;
    vector<int> switchPointsLeftGoalLines;
    vector<int> switchPointsRightGoalLines;
    vector<CustomLine> leftGoalLines;
    vector<CustomLine> rightGoalLines;
    CustomLine currentLine;
    bool noTrajectory;
};

struct GoalLineData {
	CustomLine rightGoalLine;
	CustomLine leftGoalLine;
};

class LineDetector
{
public:
    LineDetector(const Mat &f, const Config &cfg, const bool debug,
                 const int id);
    virtual ~LineDetector();
    Lines getLines();
    int detectStartLine(int dist);
    int detectStopLine(int dist);

    long time_taken_contour;
    long time_taken_find_lines;
    long time_taken_classification;
    long time_taken_filter_merge;
    long time_taken_final_filter;

    Clusters *getClusters(); // Attila: Only debugging

    // Functions to retrive debug information
    IntermediateResult_getContours *getResult_getContours();
    IntermediateResult_getRectangles *getResult_getRectangles();
    IntermediateResult *getResult_classification();
    IntermediateResult *getResult_filterAndMerge();
    IntermediateResult *getResult_finalFilter();
    LinesToUse *getResult_calculateGoalLine();
	LaneDetectorDataToDriver *getDriverData();
	FinalOutput *getResult_createTrajectory();

private:
    LineDetector(const LineDetector &);
    LineDetector &operator=(const LineDetector &);
    Line findDashLine();
    pair<Line, Line> findSolidLine(Line &dashedLine);
    void removePoint(Cluster &c, Point &p);
    int calcLength(const Point &p1, const Point &p2);
    int calcLength(const Vec4i &v);
    int calcStdev(vector<int> &v);
    Lines findCurves();
    pair<vector<Point>::iterator, vector<Point>::iterator> findBiggestDistance(Cluster &c);
    Mat getBirdView(Mat &source);
    void findLines(cv::Mat &outputImg);
    float getLineSlope(Point &p1, Point &p2);
    float getDist(const Point p1, const Point p2) const;
    int detectHorizontalLine(Mat canny_roi, int dist);
    int getRoadAngle(int lineDetected, int lineAngle);
    int getRoadSize(int roadAngle);
    Point2f getWorldPoint(Point2i imgPoint);
    int getIntersectionWithBottom(CustomLine l) const;
    int getIntersectionWithTop(CustomLine l) const;
	int getIntersectionWithY(CustomLine l, int y) const;
    CustomLine createLineFromRect(RotatedRect *rect, int sizeX, int sizeY, int polygonIndex);

	Point getLowestOrHighestPoint(std::vector<Point> pts, bool getLowest);
    //Find contours
    void getContours(cv::Mat &outputImg);
    //Get all marked lines
    void getRectangles();
    // Split big rectangles into smaller pieces
    void splitBigRectangles(int index);
    //Classify dash lines and solid lines
    void classification();
    //Filter dashes outside the solid lines and merge solid lines
    void filterAndMerge();
    //Filter lines with very small angles, filter dash positioned too high on the image or too left or too right
    void finalFilter();
    //Filer the lines w.r.t. road characteristics
    void characteristicFiltering(LinesToUse *ltu);
    // Creates a trajectory for the driver
	void manageTrajectory(LinesToUse *ltu);
    //Estimate missing needed lines
    void estimateLines(LinesToUse *ltu);
	void new_estimateLines(EstimationData *ed);
    //Calculate the goal line etc.
    void calculateGoalLine(LinesToUse *ltu);
	CustomLine new_calculateGoalLine(EstimationData *ed);

	void provideGoalLine(EstimationData *ed, GoalLineData *gld);
	CustomLine simple_calculateGoalLine(CustomLine fst, CustomLine snd, EstimationData *ed);
    
    //Split  contour at given  the points
    std::vector<RotatedRect> splitContourAtPoints(vector<Point> points,int contourIndex,bool horizontalSplit);

    //creates a PolySize
    PolySize createPolySize(const RotatedRect& rect);

    std::vector<CustomLine> findCurve(std::vector<CustomLine> lines);
	std::vector<Point> trajectorySwitchingPoints(std::vector<CustomLine> lines);
	std::vector<CustomLine> splitSolidLines(std::vector<int> cutAt, CustomLine solid);
	CustomLine getNoneCustomLine();
	bool isNoneCustomLine(CustomLine aspirant);

    cv::Mat m_frame;
    cv::Mat m_frame_color;
    cv::Mat m_frameCanny;
    Lines *m_lines;
    const bool m_debug;
    Point m_lastSolidRightTop;
    std::vector<CustomLine> detectedLines;
    Config m_config;

    vector<vector<Point> > contours_poly;
    vector<CustomLine> dashLines;
    vector<CustomLine> solidLines;
    vector<PolySize> line_sizes;
    vector<RotatedRect> rects;

    // Variables for function's results
    IntermediateResult_getContours result_getContours;
    IntermediateResult_getRectangles result_getRectangles;
    IntermediateResult result_classification;
    IntermediateResult result_filterAndMerge;
    IntermediateResult result_finalFilter;
    LinesToUse ltu;
    Lines result_getLines;
    LaneDetectorDataToDriver *dataToDriver;
    FinalOutput finalOutput;
};

}
#endif

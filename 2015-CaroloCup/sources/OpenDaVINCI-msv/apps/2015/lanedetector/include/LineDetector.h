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

namespace msv {

struct Config {
	int th1, th2, hlTh, caThVal, caThMax, caThTyp, pGain, intGain, derGain,
			houghMinAngle, houghMaxAngle, houghStartVal, houghMaxLines,
			XTimesYMin, XTimesYMax, maxY, maxArea;
};

struct PolySize {
	int sizeX, sizeY, sizeR;
	Point shortSideMiddle;
	Point longSideMiddle;
};

// Structs for intermediate results
struct IntermediateResult_getContours{
	vector<vector<Point> > contours;
};

struct IntermediateResult_getAllLines{
	vector<RotatedRect> rects;
};

struct IntermediateResult{
	vector<CustomLine> dashLines;
	vector<CustomLine> solidLines;
	int cntDash;
	int cntSolid;
	bool foundStopStartLine;
	bool intersectionOn;
	bool foundIntersection;
};


class LineDetector {
public:
	LineDetector(const Mat& f, const Config& cfg, const bool debug,
			const int id);
	virtual ~LineDetector();
	Lines getLines();
	int detectStartLine(int dist);
	int detectStopLine(int dist);

	Clusters* getClusters(); // Attila: Only debugging

	// Functions to retrive debug information
	IntermediateResult_getContours getResult_getContours();
	IntermediateResult_getAllLines getResult_getAllLines();
	IntermediateResult getResult_classification();
	IntermediateResult getResult_filterAndMerge();
	IntermediateResult getResult_finalFilter();

private:
	LineDetector(const LineDetector&);
	LineDetector& operator=(const LineDetector&);
	Line findDashLine();
	pair<Line, Line> findSolidLine(Line& dashedLine);
	void removePoint(Cluster& c, Point& p);
	int calcLength(const Point& p1, const Point& p2);
	int calcLength(const Vec4i& v);
	int calcStdev(vector<int>& v);
	Lines findCurves();
	pair<vector<Point>::iterator, vector<Point>::iterator> findBiggestDistance(
			Cluster& c);
	Mat getBirdView(Mat& source);
	CustomLine createLineFromRect(RotatedRect* rect, int sizeX, int sizeY);
	void findLines(cv::Mat &outputImg);
	float getLineSlope(Point &p1, Point &p2);
	float getDist(const Point p1, const Point p2) const;
	int detectHorizontalLine(Mat canny_roi, int dist);
	int getRoadAngle(int lineDetected, int lineAngle);
	int getRoadSize(int roadAngle);
	Point2f getWorldPoint(Point2i imgPoint);
	int getIntersectionWithBottom(CustomLine l) const;

	//Find contours
	void getContours(cv::Mat &outputImg);
	//Get all marked lines
	void getAllLines();
	//Classify dash lines and solid lines
	void classification();
	//Filter dashes outside the solid lines and merge solid lines
	void filterAndMerge();
	//Filter lines with very small angles, filter dash positioned too high on the image or too left or too right
	void finalFilter();

	cv::Mat m_frame;
	cv::Mat m_frameCanny;
	Lines* m_lines;
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
	IntermediateResult_getAllLines result_getAllLines;
	IntermediateResult result_classification;
	IntermediateResult result_filterAndMerge;
	IntermediateResult result_finalFilter;
};

}
#endif

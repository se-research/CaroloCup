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
#include "DBSCAN.h"
#include "MSAC.h"
#include <numeric>

using namespace cv;
using namespace std;

namespace carolocup {

struct Config {
  int th1, th2, hlTh, caThVal, caThMax, caThTyp,
      pGain, intGain, derGain, houghMinAngle, houghMaxAngle, 
      houghStartVal, houghMaxLines, XTimesYMin, XTimesYMax, maxY, maxArea;

};

class LineDetector {
public:
  LineDetector(const Mat& f, const Config& cfg, const bool debug, const int id);
  virtual ~LineDetector();
  Lines getLines();
  int detectStartLine(int dist);
  int detectStopLine(int dist);

  Clusters* getClusters(); // Attila: Only debugging

private:
  LineDetector(const LineDetector&);
  LineDetector& operator=(const LineDetector&);
  Line findDashLine();
  pair<Line,Line> findSolidLine(Line& dashedLine);
  void removePoint(Cluster& c, Point& p);
  int calcLength(const Point& p1, const Point& p2);
  int calcLength(const Vec4i& v);
  int calcStdev(vector<int>& v);
  Lines findCurves();
  pair<vector<Point>::iterator,vector<Point>::iterator> findBiggestDistance(Cluster& c);
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

  cv::Mat m_frame;
  cv::Mat m_frameCanny;
  Lines* m_lines;
  const bool m_debug;
  Point m_lastSolidRightTop;
  std::vector<CustomLine> detectedLines;
  Config m_config;

};

}
#endif

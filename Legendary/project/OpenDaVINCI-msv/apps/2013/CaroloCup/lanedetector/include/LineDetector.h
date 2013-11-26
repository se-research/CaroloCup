#ifndef LINEDETECTOR_H_
#define LINEDETECTOR_H_

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
  int th1, th2, hlTh, hlMaxLineGap, hlMaxLineLength, caThVal, caThMax, caThTyp,
      birdF, birdDist, birdAlpha, birdBeta, birdGamma, dbEps, dbMinPts,
      dashMin, dashMax, dashWidth, solidMin, solidWidth, pGain, intGain, derGain, speed;
};

struct CustomLine {
  Point p1, p2;
  float slope;
  bool operator < (const CustomLine& other ) const {
     return slope < other.slope;
  }
};

class LineDetector {
public:
  LineDetector(const Mat& f, const Config& cfg, const bool debug);
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
  void processImageMSAC(MSAC &msac, int numVps, cv::Mat &imgGRAY, cv::Mat &outputImg);
  float getLineSlope(Point &p1, Point &p2);
  float getDist(const Point p1, const Point p2) const;
  int detectHorizontalLine(Mat canny_roi, int dist);

  cv::Mat m_frame;
  cv::Mat m_frameCanny;
  //vector<Vec4i> m_houghLines;
  Lines* m_lines;
  Dbscan* m_clusters;
  const Config m_config;
  const bool m_debug;
  Point m_lastSolidRightTop;
  std::vector<CustomLine> detectedLines;
  CustomLine supposedMidLine;

};

}
#endif

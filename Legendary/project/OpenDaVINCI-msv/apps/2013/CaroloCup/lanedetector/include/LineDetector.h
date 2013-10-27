#ifndef LINEDETECTOR_H_
#define LINEDETECTOR_H_

#include <queue>
#include "opencv2/opencv.hpp"
#include "LineDetectorTypes.h"
#include "LaneDetectionData.h"
#include "DBSCAN.h"
#include <numeric>

using namespace cv;
using namespace std;

namespace carolocup {

struct Config {
  int th1, th2, hlTh, hlMaxLineGap, hlMaxLineLength, caThVal, caThMax, caThTyp,
      birdF, birdDist, birdAlpha, birdBeta, birdGamma, dbEps, dbMinPts,
      dashMin, dashMax, dashWidth, solidMin, solidWidth;
};

class LineDetector {
public:
  LineDetector(const Mat& f, const Config& cfg, const bool debug);
  virtual ~LineDetector();
  Lines getLines();

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
  pair<vector<Point>::iterator,vector<Point>::iterator> findBiggestDistance(Cluster& c);
  Mat getBirdView(Mat& source);

  Lines* m_lines;
  Dbscan* m_clusters;
  const Config m_config;
  const bool m_debug;
};

}
#endif

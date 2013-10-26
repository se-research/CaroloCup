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

class LineDetector {
public:
  LineDetector(vector<Vec4i>& lines, float eps, int minPts, int dashMin, int dashMax, int dashWidth, int solidMax, int solidWidth);
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

  Lines* m_lines;
  Dbscan* m_clusters;
  int m_dashMin;
  int m_dashMax;
  int m_dashWidth;
  int m_solidMax;
  int m_solidWidth;
};

}
#endif

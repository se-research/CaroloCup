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

class LineDetector {
public:
  // TODO: replace change esp to int for performanc reasons
  LineDetector(vector<Vec4i>& lines, float eps, int minPts);
  virtual ~LineDetector();
  msv::Lines getLines();

  Clusters* getClusters(); // Attila: Only debugging

private:
  Line findDashLine();
  pair<Line,Line> findSolidLine(Line& dashedLine);
  int calcLength(const Point& p1, const Point& p2);
  int calcLength(const Vec4i& v);
  int calcStdev(vector<int>& v);
  Line findBiggestDistance(Cluster& c);

  msv::Lines* m_lines;
  Dbscan* m_clusters;
  int m_stdevTh;
  int m_maxXTh;
  int m_maxyTh;
};

#endif

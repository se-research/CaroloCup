#ifndef LINEDETECTOR_H_
#define LINEDETECTOR_H_

#include <queue>
#include "opencv2/opencv.hpp"
#include "LineDetectorTypes.h"
#include "DBSCAN.h"

using namespace cv;
using namespace std;

class LineDetector {
public:
  // TODO: replace change esp to int for performanc reasons
  LineDetector(vector<Vec4i>& lines, float eps, int minPts);
  virtual ~LineDetector();
  Line* getDashLine();

  Clusters* getClusters(); // Attila: Only debugging
  /*
   * First is always the closer and second is the further line.
   */
  std::pair<Line,Line> getSolidLine();

  Line getOptimalLine();

private:
  Dbscan* m_clusters;
  Line* m_dashLine;
  Line* m_solidLine;
};

#endif

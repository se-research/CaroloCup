#ifndef DBSCAN_H_
#define DBSCAN_H_

#include "LineDetectorTypes.h"

using namespace cv;
using namespace std;

class Dbscan {
public:
  Dbscan (vector<Point> *points, float eps, int minPts);
  virtual ~Dbscan ();
  Clusters* getClusters();

private:
  int m_minPts;
  float m_eps;
  Clusters m_clusters;
  vector<int> regionQuery(vector<Point> *points, Point *keypoint, float eps);
  void  calc(vector<Point> *points);
};

#endif

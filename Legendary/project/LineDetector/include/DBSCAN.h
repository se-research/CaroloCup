#ifndef DBSCAN_H_
#define DBSCAN_H_

#include "LineDetectorTypes.h"

using namespace cv;
using namespace std;

class Dbscan {
public:
  Dbscan (vector<Point>* points, float eps, int minPts);
  virtual ~Dbscan ();
  Clusters* getClusters();

private:
  int m_minPts;
  float m_eps;
  int m_c;
  vector<Point>* m_points;
  vector<bool> m_clustered;
  vector<int> m_noise;
  vector<bool> m_visited;
  Clusters m_clusters;
  vector<int> regionQuery(Point& point);
  void expandCluster(Point& point, vector<int>& nbPoints);
  void calc();
};

#endif

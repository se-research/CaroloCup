#ifndef DBSCAN_H_
#define DBSCAN_H_

#include "LineDetectorTypes.h"

using namespace cv;
using namespace std;

class Dbscan {
public:
  Dbscan (vector<Point>* points, int eps, int minPts);
  virtual ~Dbscan ();
  Clusters* getClusters();

private:
  Dbscan (const Dbscan&);
  Dbscan& operator=(const Dbscan&);

  int m_eps;
  int m_c;
  vector<Point>* m_points;
  unsigned int m_minPts;
  vector<bool> m_clustered;
  vector<int> m_noise;
  vector<bool> m_visited;
  Clusters m_clusters;

  vector<int> regionQuery(Point& point);
  void expandCluster(Point& point, vector<int>& nbPoints);
  void calc();
};

#endif

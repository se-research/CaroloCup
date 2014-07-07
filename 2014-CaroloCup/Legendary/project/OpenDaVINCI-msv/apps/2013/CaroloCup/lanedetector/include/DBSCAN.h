#ifndef DBSCAN_H_
#define DBSCAN_H_

#include "LineDetectorTypes.h"

using namespace cv;
using namespace std;

class Dbscan {
public:
  Dbscan ( const vector<Line>* const lines, const int eps, const int minLines);
  virtual ~Dbscan ();
  Clusters* getClusters();

private:
  Dbscan (const Dbscan&);
  Dbscan& operator=(const Dbscan&);

  int m_eps;
  int m_c;
  const vector<Line>* m_lines;
  unsigned int m_minLines;
  vector<bool> m_clustered;
  vector<bool> m_visited;
  Clusters m_clusters;

  vector<int> regionQuery(const Line& line) const ;
  void expandCluster(const Line& line, vector<int>& nbLines);
  void calc();
  float getDist(const Point p1, const Point p2) const;
};

#endif

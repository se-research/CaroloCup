#include "LineDetector.h"
#include "DBSCAN.h"

using namespace std;
using namespace cv;

LineDetector::LineDetector(vector<Vec4i>& lines, float eps, int minPts)
/*: m_dashLine(0)*/
{
  vector<Point> points;

  for(vector<Vec4i>::iterator it = lines.begin(); it != lines.end(); ++it) {
    points.push_back(Point((*it)[0],(*it)[1]));
    points.push_back(Point((*it)[2],(*it)[3]));
  }

  m_clusters = new Dbscan(&points, eps, minPts);

}
LineDetector::~LineDetector(){
  delete m_clusters;
}

// Attila: Only for debugging
Clusters* LineDetector::getClusters() {
  return m_clusters->getClusters();
}

Vec4i LineDetector::getDashLine() {
  return m_dashLine;
}


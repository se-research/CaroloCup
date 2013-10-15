#include "LineDetector.h"
#include "DBSCAN.h"

using namespace std;
using namespace cv;

LineDetector::LineDetector(vector<Vec4i>& lines, float eps, int minPts)
: m_dashLine(NULL)
{
  vector<Point> points;

  for(vector<Vec4i>::iterator it = lines.begin(); it != lines.end(); ++it) {
    points.push_back(Point((*it)[0],(*it)[1]));
    points.push_back(Point((*it)[2],(*it)[3]));
  }

  // Attila: Only for debugging
  m_clusters = new Dbscan(&points, eps, minPts);

}
LineDetector::~LineDetector(){
  delete m_clusters;
}

// Attila: Only for debugging
Clusters* LineDetector::getClusters() {
  return m_clusters->getClusters();
}

Line* LineDetector::getDashLine() {
  if (m_dashLine) {
    m_dashLine = new Vec4i(10,10,10,10);
  }
  return m_dashLine;
}


#include "LineDetector.h"

using namespace std;
using namespace cv;

LineDetector::LineDetector(vector<Vec4i>& lines, float eps, int minPts)
  : m_dashLine(NULL)
  , m_solidLine(NULL)
  //, m_maxXth(10)
  //, m_maxyth(40)
  , m_stdevTh(3)
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

// Does not work right now.
Line LineDetector::getSolidLine() {
  if (!m_solidLine) {
    m_solidLine = new Vec4i( findSolidLine() );
  }
  return *m_solidLine;
}

// Does not work right now.
Line LineDetector::findSolidLine() {
  Clusters* clusters = m_clusters->getClusters();
  Line solidLine(0,0,0,0);

  for (vector<Cluster>::iterator it = clusters->begin(); it != clusters->end(); ++it) {
    int maxX = 0, maxY = 0;
    for (vector<Point>::iterator it2 = it->begin(); it2 != it->end(); ++it2) {
      for (vector<Point>::iterator it3 = it->begin(); it3 != it->end(); ++it3) {
        int x = abs(it2->x - it3->x);
        int y = abs(it2->y - it3->y);
        if (maxY < y) {
          maxY = y;
        }
        if (maxX < x) {
          maxX = x;
        }
      }
    }
    if (maxX < 10 && maxY > 40) {
    //if (maxX < m_maxXTh && maxY > m_maxYTh) {
      solidLine = findBiggestDistance(*it);
      break;
    }
  }
  return solidLine;
}

// Does not work right now.
Line LineDetector::getDashLine() {
  if (!m_dashLine) {
    m_dashLine = new Vec4i( findDashLine() );
  }
  return *m_dashLine;
}

// Does not work right now.
// TODO: 'Rectanglness' check missing
Line LineDetector::findDashLine() {
  Clusters* clusters = m_clusters->getClusters();
  Line dashLine(0,0,0,0);

  for (vector<Cluster>::iterator it = clusters->begin(); it != clusters->end(); ++it) {

    // Reclustering
    Dbscan subClusters(&*it, 10, 10);

    // A dashline cluster should contains multiple subclusters
    if ( 2 > subClusters.getClusters()->size() ) {
      continue;
    }

    Clusters* dashLineSubclusters = subClusters.getClusters();
    vector<int> maxs;

    // Find the biggest distance in the subclusters
    for (vector<Cluster>::iterator subCluster = dashLineSubclusters->begin(); subCluster != dashLineSubclusters->end(); ++subCluster) {
      int max = 0;
      for (vector<Point>::iterator it2 = subCluster->begin(); it2 != subCluster->end(); ++it2) {
        for (vector<Point>::iterator it3 = subCluster->begin(); it3 != subCluster->end(); ++it3) {
          int dist = calcLength(*it2,*it3);
          if (dist > max) {
            max = dist;
          }
        }
      }
      maxs.push_back(max);
    }

    // check if the biggest distances in subclusters are similar
    if (calcStdev(maxs) > m_stdevTh) {
      continue;
    }

    dashLine = findBiggestDistance(*it);
  }

  return dashLine;
}

//find two points with biggest distance in the whole dashLine claster
Line LineDetector::findBiggestDistance(Cluster& c){
  int max = 0;
  Vec4i retLine(0,0,0,0);
  for (vector<Point>::iterator it2 = c.begin(); it2 != c.end(); ++it2) {
    for (vector<Point>::iterator it3 = c.begin(); it3 != c.end(); ++it3) {
      int dist = calcLength(*it2,*it3);
      if (dist > max) {
        max = dist;
        retLine = Vec4i(it2->x,it2->y,it3->x,it3->y);
      }
    }
  }
  return retLine;
}

int LineDetector::calcLength(const Point& p1, const Point& p2){
  Point diff = p1 - p2;
  return sqrt(diff.x*diff.x + diff.y*diff.y);
}

int LineDetector::calcLength(const Vec4i& v){
  Point diff = Point(v[0],v[1]) - Point(v[2],v[3]);
  return sqrt(diff.x*diff.x + diff.y*diff.y);
}

int LineDetector::calcStdev(vector<int>& v){
  int sum = std::accumulate(v.begin(), v.end(), 0.0);
  int mean = sum / v.size();
  int sq_sum = std::inner_product(v.begin(), v.end(), v.begin(), 0.0);
  return std::sqrt(sq_sum / v.size() - mean * mean);
}

#include "LineDetector.h"

namespace carolocup {

using namespace std;
using namespace cv;

LineDetector::LineDetector(vector<Vec4i>& lines, float eps, int minPts, int dashMin, int dashMax, int dashWidth,int solidMax, int solidWidth)
  : m_lines(NULL)
  , m_clusters(NULL)
  , m_dashMin(dashMin)
  , m_dashMax(dashMax)
  , m_dashWidth(dashWidth)
  , m_solidMax(solidMax)
  , m_solidWidth(solidWidth)
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
  if (NULL != m_lines) {
    delete m_lines;
  }
}

// Attila: Only for debugging
Clusters* LineDetector::getClusters() {
  return m_clusters->getClusters();
}

pair<Line,Line> LineDetector::findSolidLine(Line& dashedLine) {
  Clusters* clusters = m_clusters->getClusters();
  Line solidLineLeft(0,0,0,0);
  Line solidLineRight(0,0,0,0);

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

    if (maxX < m_solidWidth*(int)clusters->size() && maxY > m_solidMax) {
      pair<vector<Point>::iterator,vector<Point>::iterator> points = findBiggestDistance(*it);
      Vec4i line = Vec4i(points.first->x,points.first->y,points.second->x,points.second->y);
      if (line[0] < dashedLine[0] && line[2] < dashedLine[2]) {
        solidLineLeft = line;
      } else if (line[0] > dashedLine[0] && line[2] > dashedLine[2]) {
        solidLineRight = line;
      }
    }
  }

  return make_pair(solidLineLeft,solidLineRight);
}

Line LineDetector::findDashLine() {
  Clusters* clusters = m_clusters->getClusters();

  vector<Point> dashCluster;
  for (vector<Cluster>::iterator it = clusters->begin(); it != clusters->end(); ++it) {
    // TODO: use min norm insteand of it2
    // Find the biggest distances
    int maxDist = 0;
    pair<int,int> x = make_pair(numeric_limits<int>::max(),0);
    pair<Point,Point> points;
    //pair<Point,Point> points = make_pair(it->begin(),it->end()); // should this initialization work too
    for (vector<Point>::iterator it2 = it->begin(); it2 != it->end(); ++it2) {
      for (vector<Point>::iterator it3 = it->begin(); it3 != it->end(); ++it3) {
        if (it2 == it3) {
          continue;
        }

        // find min and max x
        if (it2->x < x.first) {
          x.first = it2->x;
        }
        if (it3->x < x.first) {
          x.first = it3->x;
        }
        if (it2->x > x.second) {
          x.second = it2->x;
        }
        if (it3->x > x.second) {
          x.second = it3->x;
        }

        int dist = calcLength(*it2,*it3);
        if (dist > maxDist) {
          maxDist = dist;
          points = make_pair(*it2,*it3);
        }
        if (maxDist > m_dashMax) {
          break;
        }
      }
      if (maxDist > m_dashMax) {
        break;
      }
    }

    if (maxDist < m_dashMax &&
        maxDist > m_dashMin &&
        (x.second - x.first) < m_dashWidth) {
      dashCluster.push_back(points.first);
      dashCluster.push_back(points.second);
    }
  }

  if (!dashCluster.size()) {
    return Vec4i(0,0,0,0);
  }

  // TODO: Refactor the rest of this function!
  pair<vector<Point>::iterator,vector<Point>::iterator> ps = findBiggestDistance(dashCluster);
  Vec4i dashLine = Vec4i(ps.first->x,ps.first->y,ps.second->x,ps.second->y);

  Point p1 = *ps.first;
  Point p2 = *ps.second;
  while (abs(dashLine[0]-dashLine[2]) > m_dashWidth) {
    Cluster dashCluster1(dashCluster);
    Cluster dashCluster2(dashCluster);
    removePoint(dashCluster1,p1);
    removePoint(dashCluster2,p2);
    pair<vector<Point>::iterator,vector<Point>::iterator> ps1 = findBiggestDistance(dashCluster1);
    pair<vector<Point>::iterator,vector<Point>::iterator> ps2 = findBiggestDistance(dashCluster2);
    Vec4i dashLine1 = Vec4i(ps1.first->x,ps1.first->y,ps1.second->x,ps1.second->y);
    Vec4i dashLine2 = Vec4i(ps2.first->x,ps2.first->y,ps2.second->x,ps2.second->y);
    int xDist1 = abs(dashLine1[0]-dashLine1[2]);
    int xDist2 = abs(dashLine2[0]-dashLine2[2]);
    if ( xDist1 < xDist2 ) {
      dashLine = dashLine1;
      dashCluster = dashCluster1;
      p1 = *ps1.first;
      p2 = *ps1.second;
    } else {
      dashLine = dashLine2;
      dashCluster = dashCluster2;
      p1 = *ps2.first;
      p2 = *ps2.second;
    }
  }

  return dashLine;
}

void LineDetector::removePoint(Cluster& c, Point& p){
  for (vector<Point>::iterator it = c.begin(); it != c.end();) {
    if (it->x == p.x && it->y == p.y) {
      it = c.erase(it);
    } else {
      ++it;
    }
  }
}

pair<vector<Point>::iterator,vector<Point>::iterator> LineDetector::findBiggestDistance(Cluster& c){
  int max = 0;
  pair<vector<Point>::iterator,vector<Point>::iterator> ret = make_pair(c.begin(),c.end());
  for (vector<Point>::iterator it2 = c.begin(); it2 != c.end(); ++it2) {
    for (vector<Point>::iterator it3 = c.begin(); it3 != c.end(); ++it3) {
      int dist = calcLength(*it2,*it3);
      if (dist > max) {
        max = dist;
        ret = make_pair(it2,it3);
      }
    }
  }
  return ret;
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
  if (0 == v.size()) {
    return numeric_limits<int>::max();
  }
  int sum = std::accumulate(v.begin(), v.end(), 0.0);
  int mean = sum / v.size();
  int sq_sum = std::inner_product(v.begin(), v.end(), v.begin(), 0.0);
  return std::sqrt(sq_sum / v.size() - mean * mean);
}

Lines LineDetector::getLines() {
  if (NULL == m_lines) {
    Line dashed = findDashLine();
    pair<Line,Line> solid = findSolidLine(dashed);

    m_lines = new Lines(solid.first,dashed,solid.second);
  }
  return *m_lines;
}

}

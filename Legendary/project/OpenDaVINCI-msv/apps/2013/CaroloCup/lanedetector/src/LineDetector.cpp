#include "LineDetector.h"

namespace carolocup {

using namespace std;
using namespace cv;

LineDetector::LineDetector(const Mat& f, const Config& cfg, const bool debug)
  : m_lines(NULL)
  , m_clusters(NULL)
  , m_config(cfg)
  , m_debug(debug)
{
  Mat birdView, frame;

  frame = f.clone();
  cvtColor( frame, frame, CV_BGR2GRAY );
  threshold( frame, frame, m_config.caThVal, m_config.caThMax, m_config.caThTyp );
  //if (m_debug) imshow("th",frame);
  GaussianBlur( frame, frame, Size(7,7), 1.5, 1.5 );
  Canny( frame, frame, m_config.th1, m_config.th2 );
  //if (m_debug) imshow("canny",frame);

  warpPerspective(
      frame,
      birdView,
      getBirdView(frame),
      frame.size(),
      INTER_CUBIC|WARP_INVERSE_MAP);
  //if (m_debug) imshow("bird",frame);

  vector<Vec4i> lines;
  HoughLinesP(birdView, lines, 1, CV_PI/180, m_config.hlTh, m_config.hlMaxLineLength, m_config.hlMaxLineGap );

  vector<Point> points;
  for(vector<Vec4i>::iterator it = lines.begin(); it != lines.end(); ++it) {
    points.push_back(Point((*it)[0],(*it)[1]));
    points.push_back(Point((*it)[2],(*it)[3]));
  }

  m_clusters = new Dbscan(&points, m_config.dbEps, m_config.dbMinPts);
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

    if (maxX < m_config.solidWidth*(int)clusters->size() && maxY > m_config.solidMin) {
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
        if (maxDist > m_config.dashMin) {
          break;
        }
      }
      if (maxDist > m_config.dashMax) {
        break;
      }
    }

    if (maxDist < m_config.dashMax &&
        maxDist > m_config.dashMin &&
        (x.second - x.first) < m_config.dashWidth) {
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
  while (abs(dashLine[0]-dashLine[2]) > m_config.dashWidth) {
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

Mat LineDetector::getBirdView(Mat& source) {
  double alpha = ((double)m_config.birdAlpha-90.)*CV_PI/180 ;
  double beta = ((double)m_config.birdBeta-90.)*CV_PI/180 ;
  double gamma = ((double)m_config.birdGamma-90.)*CV_PI/180 ;
  double f = m_config.birdF;
  double dist = m_config.birdDist;

  double w = source.size().width;
  double h = source.size().height;

  // Projection 2D -> 3D matrix
  Mat A1=(Mat_<double>(4,3)<<
      1,0,-w/2,
      0,1,-h/2,
      0,0,0,
      0,0,1);

  // Rotation matrices around the X,Y,Z axis
  Mat RX=(Mat_<double>(4,4)<<
      1,0,0,0,
      0,cos(alpha),-sin(alpha),0,
      0,sin(alpha),cos(alpha),0,
      0,0,0,1);

  Mat RY=(Mat_<double>(4,4)<<
      cos(beta),0,-sin(beta),0,
      0,1,0,0,
      sin(beta),0,cos(beta),0,
      0,0,0,1);

  Mat RZ=(Mat_<double>(4,4)<<
      cos(gamma),-sin(gamma),0,0,
      sin(gamma),cos(gamma),0,0,
      0,0,1,0,
      0,0,0,1);

  // Composed rotation matrix with (RX,RY,RZ)
  Mat R=RX*RY*RZ ;

  // Translation matrix on the Z axis change dist will change the height
  Mat T=(Mat_<double>(4,4)<<
      1,0,0,0,
      0,1,0,0,
      0,0,1,dist,
      0,0,0,1);

  // Camera Intrisecs matrix 3D -> 2D
  Mat A2=(Mat_<double>(3,4)<<
      f,0,w/2,0,
      0,f,h/2,0,
      0,0,1,0);

  // Final and overall transformation matrix
  return A2*(T*(R*A1));
}

}

#include "DBSCAN.h"
/* DBSCAN - density-based spatial clustering of applications with noise */
// http://en.wikipedia.org/wiki/DBSCAN

using namespace cv;
using namespace std;

Dbscan::Dbscan( const vector<Line>* const lines, const int eps, const int minLines)
  : m_eps(eps)
  , m_c(0)
  , m_lines(lines)
  , m_minLines(minLines)
  , m_clustered(vector<bool>(lines->size(),false))
  , m_visited(vector<bool>(lines->size(),false))
  , m_clusters()
{
  clock_t start = clock();
  calc();
  for (unsigned int i = 0; i < m_clusters.size(); i++) {
    cout << m_clusters[i].size() << " ";
  }

  cout << "DBSCAN Time(ms) clusterSize:"<< m_clusters.size()<<" (lines: " << lines->size() << "): ";
  cout << (difftime(clock(), start) / 1000) << endl;
}

Dbscan::~Dbscan() {}

Clusters* Dbscan::getClusters() {
  return &m_clusters;
}

void Dbscan::calc() {

  //for each unvisted line P in dataset lines
  for(unsigned int i = 0; i < m_lines->size(); i++) {
    vector<int> neighborLines;

    if(m_visited[i]) {
      continue;
    }

    //Mark P as visited
    m_visited[i] = true;
    neighborLines = regionQuery(m_lines->at(i));

    if(neighborLines.size() >= m_minLines) {
      m_clusters.push_back(vector<Point>());
      expandCluster(m_lines->at(i),neighborLines);
      m_c++;
    }
  }
}

void Dbscan::expandCluster(const Line& line, vector<int>& neighborLines) {
  // add P to cluster c
  m_clusters[m_c].push_back(Point(line[0],line[1]));
  m_clusters[m_c].push_back(Point(line[2],line[3]));

  //for each line P' in neighborLines
  for(unsigned int j = 0; j < neighborLines.size(); j++) {

    //if P' is not visited
    if(!m_visited[neighborLines[j]]) {
      vector<int> neighborLines_;
      //Mark P' as visited
      m_visited[neighborLines[j]] = true;
      neighborLines_ = regionQuery(m_lines->at(neighborLines[j]));
      if(neighborLines_.size() >= m_minLines) {
        neighborLines.insert(neighborLines.end(),neighborLines_.begin(),neighborLines_.end());
      }
    }
  }

  // deleting duplicated lines
  vector<int>::iterator it;
  std::sort(neighborLines.begin(),neighborLines.end());
  it = unique(neighborLines.begin(), neighborLines.end());
  neighborLines.resize( std::distance(neighborLines.begin(),it) );

  for(unsigned int j = 0; j < neighborLines.size(); j++) {
    if(!m_clustered[neighborLines[j]]) {
      Point p1(m_lines->at(neighborLines[j])[0],m_lines->at(neighborLines[j])[1]);
      Point p2(m_lines->at(neighborLines[j])[2],m_lines->at(neighborLines[j])[3]);
      m_clusters[m_c].push_back(p1);
      m_clusters[m_c].push_back(p2);
    }
  }
}

vector<int> Dbscan::regionQuery(const Line& l) const {
  vector<int> retLines;

  for(unsigned int i = 0; i< m_lines->size(); i++) {

    Line ll = m_lines->at(i);

    float dist1 = getDist(Point(l[0], l[1]), Point(ll[0],ll[1]));
    float dist2 = getDist(Point(l[0], l[1]), Point(ll[2],ll[3]));
    float dist3 = getDist(Point(l[2], l[3]), Point(ll[0],ll[1]));
    float dist4 = getDist(Point(l[2], l[3]), Point(ll[2],ll[3]));

    if( (dist1 <= m_eps && dist1 > 0) ||
        (dist2 <= m_eps && dist2 > 0) ||
        (dist3 <= m_eps && dist3 > 0) ||
        (dist4 <= m_eps && dist4 > 0) ){
      retLines.push_back(i);
    }
  }
  return retLines;
}

float Dbscan::getDist(const Point p1, const Point p2) const {
    return sqrt(pow(p1.x-p2.x, 2) + pow(p1.y-p2.y, 2));
}


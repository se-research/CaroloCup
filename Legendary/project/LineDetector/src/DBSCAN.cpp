#include "DBSCAN.h"
/* DBSCAN - density-based spatial clustering of applications with noise */
// http://en.wikipedia.org/wiki/DBSCAN

#include <vector>

using namespace cv;
using namespace std;

Dbscan::Dbscan(vector<Point>* points, float eps, int minPts)
  : m_eps(eps)
  , m_c(0)
  , m_points(points)
  , m_minPts(minPts) {

  m_clusters.push_back(vector<Point>()); //will stay empty?

  for(int p = 0; p < m_points->size(); p++) {
    m_clustered.push_back(false);
    m_visited.push_back(false);
  }

  calc();
}

Dbscan::~Dbscan() {}

Clusters* Dbscan::getClusters() {
  return &m_clusters;
}

void Dbscan::calc() {

  //for each unvisted point P in dataset points
  for(int i = 0; i < m_points->size(); i++) {
    vector<int> neighborPts;

    if(m_visited[i]) {
      continue;
    }

    //Mark P as visited
    m_visited[i] = true;
    neighborPts = regionQuery(m_points->at(i));

    if(neighborPts.size() < m_minPts) {
      //Mark P as Noise
      m_noise.push_back(i); // All the neighbour should be noise
    } else {
      m_clusters.push_back(vector<Point>());
      expandCluster(m_points->at(i),neighborPts);
      m_c++;
    }
  }
}
//bool sortNeighbours(Point p1, Point p2){
  //return (p1.x > p2.x) || ((p1.x == p2.x)&&(p1.y > p2.y));
//}

void Dbscan::expandCluster(Point& point, vector<int>& neighborPts) {
  // add P to cluster c
  m_clusters[m_c].push_back(point);

  //for each point P' in neighborPts
  for(int j = 0; j < neighborPts.size(); j++) {
    bool reset = false;

    //if P' is not visited
    if(!m_visited[neighborPts[j]]) {
      vector<int> neighborPts_;
      //Mark P' as visited
      m_visited[neighborPts[j]] = true;
      neighborPts_ = regionQuery(m_points->at(neighborPts[j]));
      if(neighborPts_.size() >= m_minPts) {

        // Getting the union of the two neighbour vectors, TODO: optimize!!!
        vector<int> unionNbPts(neighborPts_.size()+neighborPts.size());
        sort(neighborPts.begin(), neighborPts.end());
        sort(neighborPts_.begin(), neighborPts_.end());
        vector<int>::iterator it = set_union(neighborPts.begin(), neighborPts.end(), neighborPts_.begin(), neighborPts_.end(), unionNbPts.begin());
        unionNbPts.resize(it-unionNbPts.begin());
        neighborPts.assign(unionNbPts.begin(),unionNbPts.end());
        reset = true; // OPTIMIZE!!! SLOWS DOWN EVERYTHING!!!
      }
    }

    // if P' is not yet a member of any cluster
    // add P' to cluster c
    if(!m_clustered[neighborPts[j]]) {
      m_clusters[m_c].push_back(m_points->at(neighborPts[j]));
    }

    if (reset == true) {
      j = 0;
    }
  }
}

vector<int> Dbscan::regionQuery(Point& point) {
  float dist;
  vector<int> retPoints;
  for(int i = 0; i< m_points->size(); i++) {
    dist = sqrt(pow((point.x - m_points->at(i).x),2)+pow((point.y - m_points->at(i).y),2));
    if(dist <= m_eps && dist != 0.0f) {
      retPoints.push_back(i);
    }
  }
  return retPoints;
}


#include "DBSCAN.h"
/* DBSCAN - density-based spatial clustering of applications with noise */
// http://en.wikipedia.org/wiki/DBSCAN

#include <vector>

using namespace cv;
using namespace std;

Dbscan::Dbscan(vector<Point> *points, float eps, int minPts)
  : m_eps(eps)
  , m_minPts(minPts) {
  calc(points);
}

Dbscan::~Dbscan() {}

Clusters* Dbscan::getClusters() {
  return &m_clusters;
}

void Dbscan::calc(vector<Point> *points)
{
  vector<bool> clustered;
  vector<int> noise;
  vector<bool> visited;
  vector<int> neighborPts;
  vector<int> neighborPts_;
  int c;

  int noKeys = points->size();

  //init clustered and visited
  for(int k = 0; k < noKeys; k++)
  {
    clustered.push_back(false);
    visited.push_back(false);
  }

  //C =0;
  c = 0;
  m_clusters.push_back(vector<Point>()); //will stay empty?

  //for each unvisted point P in dataset points
  for(int i = 0; i < noKeys; i++)
  {
    if(!visited[i])
    {
      //Mark P as visited
      visited[i] = true;
      neighborPts = regionQuery(points,&points->at(i),m_eps);
      if(neighborPts.size() < m_minPts) {
        //Mark P as Noise
        noise.push_back(i);
      }
      else {
        m_clusters.push_back(vector<Point>());
        c++;
        //expand cluster
        // add P to cluster c
        m_clusters[c].push_back(points->at(i));
        //for each point P' in neighborPts
        for(int j = 0; j < neighborPts.size(); j++)
        {
          //if P' is not visited
          if(!visited[neighborPts[j]])
          {
            //Mark P' as visited
            visited[neighborPts[j]] = true;
            neighborPts_ = regionQuery(points,&points->at(neighborPts[j]),m_eps);
            if(neighborPts_.size() >= m_minPts)
            {
              neighborPts.insert(neighborPts.end(),neighborPts_.begin(),neighborPts_.end());
            }
          }
          // if P' is not yet a member of any cluster
          // add P' to cluster c
          if(!clustered[neighborPts[j]])
            m_clusters[c].push_back(points->at(neighborPts[j]));
        }
      }
    }
  }
}

vector<int> Dbscan::regionQuery(vector<Point> *points, Point *keypoint, float m_eps)
{
  float dist;
  vector<int> retKeys;
  for(int i = 0; i< points->size(); i++)
  {
    dist = sqrt(pow((keypoint->x - points->at(i).x),2)+pow((keypoint->y - points->at(i).y),2));
    if(dist <= m_eps && dist != 0.0f)
    {
      retKeys.push_back(i);
    }
  }
  return retKeys;
}


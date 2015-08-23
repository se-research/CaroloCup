#ifndef LINEDETECTORTYPES_H_
#define LINEDETECTORTYPES_H_

#include "opencv2/opencv.hpp"
#include "vector"

typedef std::vector<cv::Point> Cluster;
typedef std::vector<Cluster> Clusters;

typedef cv::Vec4i Line;

#endif


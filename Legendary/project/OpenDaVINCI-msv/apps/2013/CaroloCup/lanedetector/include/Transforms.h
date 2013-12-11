#ifndef TRANSFORMS_H_
#define TRANSFORMS_H_

#include "opencv2/core/core.hpp"
//#include "opencv2/imgproc/imgproc.hpp"
using namespace cv;

typedef struct CameraStruct_ {
	float focal;
	float u0, v0;
	float height, theta0, gamma0;
	Size size;
} CameraStruct;

Point2f ipm(Point img_pt, CameraStruct cam);

#endif
/*
 * Transforms.h
 *
 *  Created on: 14 dec 2013
 *      Author: fredrik
 */

#ifndef TRANSFORMS_H_
#define TRANSFORMS_H_
#include "opencv2/core/core.hpp"
#include <math.h>

namespace carolocup {

    using namespace cv;

    typedef struct CameraStruct_ {
        float focal;
        float u0, v0;
        float height, length, theta0, gamma0;
        Size size;
    } CameraStruct;

    Point2f ipm(Point2i img_pt, CameraStruct cam);
    Point2f ipm2(Point2i img_pt, CameraStruct cam);
    Point2f ipm3(Point2i img_pt, CameraStruct cam);
    Mat getBirdTransMatrix(CameraStruct cam);
}
#endif /* TRANSFORMS_H_ */

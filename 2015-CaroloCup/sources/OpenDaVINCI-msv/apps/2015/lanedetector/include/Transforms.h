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

namespace msv {

    using namespace cv;

    typedef struct CameraStruct_ {
        float focal, focal2;
        float u0, v0;
        float height, length, alpha, beta, gamma;
        Size size;
        CameraStruct_()
        	: focal()
			, focal2()
        	, u0()
        	, v0()
			, height()
			, length()
			, alpha()
			, beta()
			, gamma()
			, size()
			{  }
    } CameraStruct;

    Point2f ipm(Point2i img_pt, CameraStruct cam);
    Point2f ipm2(Point2i img_pt, CameraStruct cam);
    Point2f ipm3(Point2i img_pt, CameraStruct cam);
    Mat getBirdTransMatrix(CameraStruct cam);
    Point2f ipmFinal(Point2i p, CameraStruct cam);
}
#endif /* TRANSFORMS_H_ */

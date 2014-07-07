/*
 * RoadModel.h
 *
 *  Created on: Apr 8, 2013
 *      Author: Oscar Muchow
 */

#ifndef ROADMODEL_H_
#define ROADMODEL_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <opencv/cv.h>
#include "core/SharedPointer.h"
#include "core/base/ConferenceClientModule.h"
#include "core/wrapper/SharedMemory.h"

#include <iostream>

namespace msv {

using namespace cv;
using namespace std;

class RoadModel {
private:
	double steeringAngle;
	double oldAngle;
	int roadMiddle;

public:
	RoadModel();
	virtual ~RoadModel();
	double findSteeringAngle(Mat &grayImage, int &straight, int &curved, int &noline);
	bool findIntersection(Mat &grayImage);

private:
	Point pointSearchRight(Mat &image, int y);
	Point pointSearchLeft(Mat &image, int y);
};

} // end msv
#endif /* ROADMODEL_H_ */

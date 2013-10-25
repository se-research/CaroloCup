/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <opencv/cv.h>
#include <opencv/highgui.h>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/image/SharedImage.h"
#include "core/io/ContainerConference.h"
#include "core/wrapper/SharedMemoryFactory.h"
#include "LaneDetectionData.h"

#include "LaneDetector.h"
#include <stdio.h>
#include <math.h>

namespace carolocup {

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::image;

double angle = 90;
double old_degree = 90;
CvFont font1;
int counterImg = 0;
double pastAngles[2] = { 90, 90 };
int pixelsFromTop=320;
int counterPastAngles = 0;
//int a = 0;
//int b = 0;
//int c = 0;
enum LANE_STATES {
	STRAIGHT_STATE = 0, LEFT_STATE = -1, RIGHT_STATE = 1
} state;

LaneDetector::LaneDetector(const int32_t &argc, char **argv) :
									ConferenceClientModule(argc, argv, "lanedetector"),
									m_hasAttachedToSharedImageMemory(false), m_sharedImageMemory(),
									m_image(NULL), color_dst(NULL), dst(NULL), storage(NULL),
									m_cameraId(-1), m_debug(false) {
}

LaneDetector::~LaneDetector() {
}

void LaneDetector::setUp() {
	// This method will be call automatically _before_ running body().

	if (m_debug) {
		// Create an OpenCV-window.
		cvNamedWindow("WindowShowImage", CV_WINDOW_AUTOSIZE);
		cvMoveWindow("WindowShowImage", 300, 100);
	}

	//Fonts Set Up
	initFont();
}

void LaneDetector::tearDown() {
	// This method will be call automatically _after_ return from body().

	if (m_image != NULL) {
		cvReleaseImage(&m_image);
	}
	if (color_dst != NULL) {
		cvReleaseImage(&color_dst);
	}
	if (dst != NULL) {
		cvReleaseImage(&dst);
	}
	if (storage != NULL) {
		cvReleaseMemStorage(&storage);
	}

	if (m_debug) {
		cvDestroyWindow("WindowShowImage");
	}
}

bool LaneDetector::readSharedImage(Container &c) {
	bool retVal = false;

	if (c.getDataType() == Container::SHARED_IMAGE) {
		SharedImage si = c.getData<SharedImage> ();

		// Check if we have already attached to the shared memory.
		if (!m_hasAttachedToSharedImageMemory) {
			m_sharedImageMemory
			= core::wrapper::SharedMemoryFactory::attachToSharedMemory(
					si.getName());
		}

		// Check if we could successfully attach to the shared memory.
		if (m_sharedImageMemory->isValid()) {
			//cerr << "Got image: LOG 0.2 " << si.toString() << endl;

			// Lock the memory region to gain exclusive access. REMEMBER!!! DO NOT FAIL WITHIN lock() / unlock(), otherwise, the image producing process would fail.
			m_sharedImageMemory->lock();
			{
				// Here, do something with the image. For example, we simply show the image.

				const uint32_t numberOfChannels = 3;
				// For example, simply show the image.
				if (m_image == NULL) {
					m_image = cvCreateImage(cvSize(si.getWidth(),
							si.getHeight()), IPL_DEPTH_8U, numberOfChannels);
				}

				// Copying the image data is very expensive...
				if (m_image != NULL) {
					memcpy(m_image->imageData,
							m_sharedImageMemory->getSharedMemory(),
							si.getWidth() * si.getHeight() * numberOfChannels);
				}
			}

			// Release the memory region so that the image produce (i.e. the camera for example) can provide the next raw image data.
			m_sharedImageMemory->unlock();

			// Mirror the image.
			cvFlip(m_image, 0, -1);

			retVal = true;
		}
	}
	return retVal;
}

void LaneDetector::processImage() {
	TimeStamp currentTime_strt1;
	if (storage == NULL) {
		storage = cvCreateMemStorage(0);
	}
	cvClearMemStorage( storage);

	if (dst == NULL) {
		dst = cvCreateImage(cvGetSize(m_image), 8, 1);
	}
	if (color_dst == NULL) {
		color_dst = cvCreateImage(cvGetSize(m_image), 8, 3);
	}

	cvCvtColor(m_image, dst, CV_BGR2GRAY);
	cvThreshold(dst, dst, 210, 255, CV_THRESH_BINARY);
	cvCvtColor(dst, color_dst, CV_GRAY2BGR);
	CvScalar pixelColorRight;
	CvScalar pixelColorLeft;
	double xPurple = 0, yPurple = 0, xYellow = 0, yYellow = 0;
	double sumDegreeBoth = 0;
	double sumDegreeOne = 0;
	CvPoint* middleBoth = new CvPoint[15];
	CvPoint* middleOne = new CvPoint[15];
	CvPoint* intersec = new CvPoint[3];
	CvPoint* middleNew = new CvPoint[2];
	middleNew[0] = cvPoint( color_dst->width/2 , color_dst->height - 1);
	int indexBoth = 0;
	int indexOne = 0;
	int countBoth = 0; // countBoth if both sides of line is exist
	int countOne = 0;// countBoth if one sides of line is exist and the other missed
	int countMissedLine = 0;// both line side missed
	int leftline = 0;
	int rightline = 0;
	CvScalar colorForLines;
	//White point x coordinates
	int rightx = 0, leftx = 0;

	//Create vertical lines to identify the Intersection
	int indexInter = 0;
	CvScalar pixelColorTop;

	for (int x = (color_dst->width / 2) - 70; x < (color_dst->width / 2) + 71; x
	+= 70) {
		int topy = color_dst->height - 1;
		for (int y = color_dst->height - 1; y > 310; y--) {
			pixelColorTop = cvGet2D(color_dst, y, x);
			int whitepixel = pixelColorTop.val[0];
			if (whitepixel >= 200) {
				topy = y;
				break;
			} else {
				topy = color_dst->height - 1;
			}
		}
		intersec[indexInter] = cvPoint((int) x, (int) topy);
		indexInter++;
		if (m_debug) {
			colorForLines = CV_RGB(55, 0, 55);
			drawline(color_dst, x, x, color_dst->height - 1, topy,
					colorForLines);
		}
	}
	//Create horizontal lines
	for (int y = color_dst->height - 10; y > pixelsFromTop; y = y - 10) {
		//Start from the middle-->right and search the first white spot
		for (int x = (color_dst->width / 2) + (70 * state); x
		< color_dst->width - 1; x++) {
			pixelColorRight = cvGet2D(color_dst, y, x);
			int whitepixelr = pixelColorRight.val[0];
			if (whitepixelr >= 200) {
				rightx = x;
				break;
			} else {
				rightx = color_dst->width;
			}
		}

		//Start from the middle-->left and search the first white spot
		for (int x = (color_dst->width / 2) + (70 * state); x > 0; x--) {
			pixelColorLeft = cvGet2D(color_dst, y, x);
			int whitepixell = pixelColorLeft.val[0];
			if (whitepixell >= 200) {
				leftx = x;
				break;
			} else {
				leftx = 0;
			}
		}
		if (leftx == 0 && rightx == color_dst->width) {
			colorForLines = CV_RGB(255, 0, 0);
			countMissedLine++;
		} else if (leftx == 0 || rightx == color_dst->width) {
			middleOne[indexOne] = cvPoint((int) (rightx + leftx) / 2, y);
			xPurple = xPurple + (rightx + leftx) / 2;
			yPurple = yPurple + y;
			countOne++;
			indexOne++;

			if (leftx == 0) {
				leftline++;
			} else if (rightx == color_dst->width) {
				rightline++;
			}
			colorForLines = CV_RGB(255, 0, 255);
		}
		if (leftx != 0 && rightx != color_dst->width) {
			middleBoth[indexBoth] = cvPoint((int) (rightx + leftx) / 2, y);
			xYellow = xYellow + (rightx + leftx) / 2;
			yYellow = yYellow + y;
			indexBoth++;
			countBoth++;
			colorForLines = CV_RGB(255, 255, 0);
		}

		if (m_debug) {
			drawline(color_dst, rightx, leftx, y, y, colorForLines);
		}
	}

	if (countBoth > 1) {
		cerr << "Case : Yellow"<<endl;

		//Calculate the angle of the trajectory
		sumDegreeBoth = calcSumDegree(countBoth, middleBoth);
		angle = sumDegreeBoth / (countBoth - 1);

		//Change the state of the road
		if (angle < 80) {
			state = LEFT_STATE;
		} else if (angle > 100 && angle < 360) {
			state = RIGHT_STATE;
		} else
			state = STRAIGHT_STATE;
		cerr << "Angle state " << angle << endl;

		switch (abs(state)) {

		case 0: {
			middleNew[1] = cvPoint((int) xYellow/countBoth , yYellow/countBoth);
			sumDegreeBoth = calcSumDegree(2, middleNew);
			angle = sumDegreeBoth;
angle = (angle +(pastAngles[0] + pastAngles[1])/ 2)/2;
			pastAngles[counterPastAngles] = angle;
			counterPastAngles++;
			if (counterPastAngles > 1) {
				counterPastAngles = 0;
			}
            pixelsFromTop = 320;
			break;
		}

		case 1: {
			middleNew[1] = cvPoint((int) xYellow/countBoth , yYellow/countBoth);
			sumDegreeBoth = calcSumDegree(2, middleNew);
			angle = sumDegreeBoth;
angle = (angle +(pastAngles[0] + pastAngles[1])/ 2)/2;
			pastAngles[counterPastAngles] = angle;
			counterPastAngles++;
			if (counterPastAngles > 1) {
				counterPastAngles = 0;
			}
pixelsFromTop = 320;
			break;
		}

		}
	} else if (countOne > 1) {
		cerr << "Case : Purple" << endl;
		sumDegreeOne = calcSumDegree(countOne, middleOne);
		angle = sumDegreeOne / (countOne - 1);

		switch (abs(state)) {

		case 0: {
			angle = ((pastAngles[0] + pastAngles[1]) / 2);
pixelsFromTop = 320;
			break;
		}

		case 1: {
            middleNew[1] = cvPoint((int) xPurple/countOne , yPurple/countOne);
            sumDegreeOne = calcSumDegree(2, middleNew);
            angle = sumDegreeOne;
				angle = (angle +(pastAngles[0] + pastAngles[1])/ 2)/2;

			pastAngles[counterPastAngles] = angle;
			counterPastAngles++;
			if (counterPastAngles > 1) {
				counterPastAngles = 0;
			}
pixelsFromTop = 320;
			break;
		}

		}

	}else {
		cerr << "Case : RED" <<endl;

		switch (abs(state)) {

		case 0: {
			angle = 90;
pixelsFromTop = 220;
			break;
		}

		case 1: {
			angle = ((pastAngles[0] + pastAngles[1]) / 2);
pixelsFromTop = 220;
			break;
		}

		}
	}

	//If the vertical points has a small difference an intersection
	//is present and we send to the driver angle of 400 degree
/*	if (intersec[0].y != color_dst->height - 1 && intersec[1].y
			!= color_dst->height - 1 && intersec[2].y != color_dst->height - 1
			&& abs(intersec[0].y - intersec[1].y) < 10 && abs(intersec[1].y
					- intersec[2].y) < 10 && abs(intersec[2].y - intersec[0].y) < 10) {
   /     a

	}
*/
	cerr << "Angle : " << angle << endl;
	cerr << "State " << state << endl;

	// Create an instance of your data structure and set some values.
  const Lines dummyLines(Vec4i(0,0,0,0),Vec4i(0,0,0,0),Vec4i(0,0,0,0));
	LaneDetectionData data;
  data.setLaneDetectionData(dummyLines);

	// Create a container from your data structure so that it can be transferred.
	// _remember_ the assigned ID (here 101): It must be used by the receiver to read the data successfully.
	// The ID must by from within this range: 0-127.
	Container con(Container::USER_DATA_1, data);

	// Send the data:
	getConference().send(con);

	// // Create an instance of  data structure for parking and set some values.

	if (m_debug) {
		// Show the image
		if (color_dst != NULL) {
//			cvShowImage("WindowShowImage", color_dst);
//			cvWaitKey(10);
        if (counterImg%10==0){
            stringstream str;
            str << setfill('0');
            str << "/tmp/img" << setw(5) << counterImg << ".jpg";
            cvSaveImage(str.str().c_str(), color_dst );

        }
        counterImg++;
		}
	}

	delete[](middleBoth);
	delete[](middleOne);
	delete[](intersec);
delete[] (middleNew);

	TimeStamp currentTime_strt7;
	double timeStep_total = (currentTime_strt7.toMicroseconds() - currentTime_strt1.toMicroseconds()) / (1000.0 * 1000.0);
	cout << "Total  "<< timeStep_total << endl;

}

// Methods

void LaneDetector::drawline(IplImage* _color_dst, int maxXR, int maxXL,
		int maxYR, int maxYL, CvScalar colorForLines) {
	initFont();
	CvScalar red = CV_RGB(255, 0, 0);

	char xy[255];
	sprintf(xy, "(% d, %d)", (int) maxXL, (int) maxYL);

	char xy1[255];
	sprintf(xy1, "(% d, %d)", (int) maxXR, (int) maxYR);
	char dst1[255];
	sprintf(dst1, "%d", (int) 320 - maxXL);
	char dst2[255];
	sprintf(dst2, "%d", (int) maxXR - 320);

	const char* text = "o";
	char dst3[255];
	sprintf(dst3, "%d, %d", (int) (maxXR + maxXL) / 2, (int) maxYR);

	//dot
	int m = (int) (maxXR + maxXL) / 2;
	cvPutText(_color_dst, text, cvPoint(m, maxYR), &font1, red);

	cvLine(_color_dst, cvPoint(maxXL, maxYL), cvPoint(maxXR, maxYR),
			colorForLines, 1, 8);//CV_RGB(128, 255, 255)


	maxYR = 0;
	maxXR = 0;

}

void LaneDetector::initFont() {
	double hscale1 = 0.4;
	double vscale1 = 0.3;
	double shear1 = 0.2;
	int thickness1 = 1;
	int line_type1 = 6;

	cvInitFont(&font1, CV_FONT_HERSHEY_DUPLEX, hscale1, vscale1, shear1,
			thickness1, line_type1);

}

double LaneDetector::calcSumDegree(int count, CvPoint* middle) {

	double theta = 0;
	double slope = 0;
	double dx = 0;
	double dy = 0;
	double degree = 0;
	double sumDegree = 0;
	for (int j = 0; j < count - 1; j++) {
		dx = (middle[j].x - middle[j + 1].x);// x coordinate difference
		dy = (middle[j].y - middle[j + 1].y);// y coordinate difference

		// quadrant I
		if (dx > 0 && dy >= 0) {
			slope = dy / dx;
			theta = atan(slope);
			degree = (theta) * 180 / CV_PI;
		} else if (dx < 0 && dy > 0) {
			// quadrant II
			slope = dy / dx;
			theta = atan(slope);
			degree = (theta) * 180 / CV_PI + 180;
		} else if (dx < 0 && dy <= 0) {
			// quadrant III
			slope = dy / dx;
			theta = atan(slope);
			degree = (theta) * 180 / CV_PI + 180;

		} else if (dx > 0 && dy < 0) {
			// quadrant IV
			slope = dy / dx;
			theta = atan(slope);
			degree = (theta) * 180 / CV_PI + 360;

		} else if (middle[j].x == middle[j + 1].x) {
			degree = 90;
		}

		sumDegree = sumDegree + degree;
	}

	return sumDegree;

}

string LaneDetector::getDirection(const double &ang) const {
	string text = "";

	// it is not finished just it is trial and error
	// if angle is between 85 && 95 - go straight
	// <85 - turn right
	// >95 turn left
	if (ang >= 80 && ang <= 97) {
		text = "\tstraight";
	} else if (ang < 80 && ang > 60) {
		text = "left";
	} else if (ang > 45 && ang <= 60) {
		text = "left";
	} else if (ang <= 45) {
		text = "left";
	} else if (ang > 97 && ang <= 120) {
		text = "\t\tright";
	} else if (ang < 150 && ang > 120) {
		text = "\t\tright";
	} else if (ang < 180 && ang >= 150) {
		text = "\t\tright";
	}
	return text;
}

void LaneDetector::printText(double ang, IplImage* _color_dst) {
	string text = getDirection(ang);
	cvPutText(_color_dst, text.c_str(), cvPoint(150, 20), &font1, CV_RGB(255,
			255, 255));
}
double LaneDetector::calcSumDistanceFromCenter_for_Parking(int count,
		CvPoint* distanceFromCenter) {
	double sumDistance = 0;
	for (int j = 0; j < count - 1; j++) {
		sumDistance = sumDistance + distanceFromCenter[j].x;
	}
	return sumDistance;
}

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE LaneDetector::body() {
	// Get configuration data.
	KeyValueConfiguration kv = getKeyValueConfiguration();
	m_cameraId = kv.getValue<int32_t> ("lanedetector.camera_id");
	m_debug = kv.getValue<int32_t> ("lanedetector.debug") == 1;
	bool use_real_camera = true;

	// Try to open the camera device.
	CvCapture *capture = cvCaptureFromCAM(m_cameraId);
	if (!capture) {
		cerr << "Could not open real camera; falling back to SHARED_IMAGE."
				<< endl;
		use_real_camera = false;
	}

	while (getModuleState() == ModuleState::RUNNING) {
		bool has_next_frame = false;

		// Use the shared memory image.
		if (!use_real_camera) {
			// Get the most recent available container for a SHARED_IMAGE.
			Container c = getKeyValueDataStore().get(Container::SHARED_IMAGE);

			if (c.getDataType() == Container::SHARED_IMAGE) {
				// Example for processing the received container.
				has_next_frame = readSharedImage(c);
			}
		} else {
			// Use the real camera.
			if (cvGrabFrame(capture)) {
				m_image = cvRetrieveFrame(capture);
				has_next_frame = true;
			}
		}

		// Process the read image.
		if (true == has_next_frame) {
			processImage();
		}

		if (use_real_camera) {
			// Unset m_image only for the real camera to avoid memory leaks.
			m_image = NULL;
		}
	}

	if (capture != NULL) {
		cvReleaseCapture(&capture);
	}

	return ModuleState::OKAY;

}

} // carolocup

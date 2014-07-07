/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <vector>

#include <opencv2/highgui/highgui.hpp>
#include <opencv2/video/tracking.hpp>

#include "core/data/TimeStamp.h"

#include "Example6.h"

namespace mousetracker {

    using namespace std;
    using namespace cv;
    using namespace core::base;
    using namespace core::data;

    struct MouseCoordinates { int x, y; double dx, dy; } mouseReadings, mouseReadingsOld;

    void mouseMoveEvent(int /*event*/, int x, int y, int /*flags*/, void* /*param*/) {
	    mouseReadings.x = x;
	    mouseReadings.y = y;
        mouseReadings.dx = (mouseReadingsOld.x - x);
        mouseReadings.dy = (mouseReadingsOld.y - y);
        
        mouseReadingsOld = mouseReadings;
    }

    MouseTracker::MouseTracker(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "MouseTracker") {}

    void MouseTracker::setUp() {}

    void MouseTracker::tearDown() {}

    void MouseTracker::drawCross(Mat &img, const CvPoint &point, const double d, const uint32_t r, const uint32_t g, const uint32_t b) {
        line(img, cvPoint(point.x - d, point.y - d), cvPoint(point.x + d, point.y + d), CV_RGB(r, g, b), 1, 8, 0);
        line(img, cvPoint(point.x + d, point.y - d), cvPoint(point.x - d, point.y + d), CV_RGB(r, g, b), 1, 8, 0);
    }

    ModuleState::MODULE_EXITCODE MouseTracker::body() {
        Mat img(WIDTH, HEIGHT, CV_8UC3);
        namedWindow("MouseTracker");
        setMouseCallback("MouseTracker", mouseMoveEvent, 0);

        vector<Point> listOfMousePoints;
        vector<Point> listOfPredictedPoints;
        vector<Point> listOfEstimatedPoints;

        // Initialize mouseReadings struct.
        mouseReadingsOld.x = 0;
        mouseReadingsOld.y = 0;

        mouseReadings.x = 0;
        mouseReadings.y = 0;
        mouseReadings.dx = 0;
        mouseReadings.dy = 0;

        const float processNoise = 1e-4; // Influence of process errors: The smaller this parameter the greater is the distance between measured and estimated point.
        const float measurementNoise = 1e-1; // Influence of measurement errors: The smaller this parameter the greater is the influence of the measured points. 
        const float errorCovariance = 1e-1;

        KalmanFilter KF(4 /*Number of dynamic parameters.*/, 4 /*Number of measured parameters.*/, 0 /*Number of control parameters.*/);
        KF.statePre.at<float>(0) = mouseReadings.x;
        KF.statePre.at<float>(1) = mouseReadings.y;
        KF.statePre.at<float>(2) = mouseReadings.dx;
        KF.statePre.at<float>(3) = mouseReadings.dy;
        KF.transitionMatrix = *(Mat_<float>(4, 4) << 1,0,1,0,   0,1,0,1,  0,0,1,0,  0,0,0,1);

        setIdentity(KF.measurementMatrix);
        setIdentity(KF.processNoiseCov, Scalar::all(processNoise));
        setIdentity(KF.measurementNoiseCov, Scalar::all(measurementNoise));
        setIdentity(KF.errorCovPost, Scalar::all(errorCovariance));

    	while (getModuleState() == ModuleState::RUNNING) {
            // Predict the next state.
            Mat predicted = KF.predict();

            // Store for plotting predicted data.
	        Point predictedPoint(predicted.at<float>(0), predicted.at<float>(1));
	        listOfPredictedPoints.push_back(predictedPoint);

            // Set the next measurement from mouse.
            Mat measurement(4, 1, CV_32F);
            measurement.at<float>(0) = mouseReadings.x;
	        measurement.at<float>(1) = mouseReadings.y;
	        measurement.at<float>(2) = mouseReadings.dx;
	        measurement.at<float>(3) = mouseReadings.dy;

            // Store for plotting real measurements.
	        Point measuredPoint(measurement.at<float>(0), measurement.at<float>(1));
	        listOfMousePoints.push_back(measuredPoint);

            // Correct Kalman filter by measurement.
	        Mat estimation = KF.correct(measurement);

            // Store for plotting estimation points.
	        Point estimatedPoint(estimation.at<float>(0), estimation.at<float>(1));
	        listOfEstimatedPoints.push_back(estimatedPoint);
	
            // Clear current image to have a nice visualization of where the estimated and measured mouse pointers are located.
            img = Scalar::all(0);
            drawCross(img, measuredPoint, 5, 255, 255, 255);
            drawCross(img, estimatedPoint, 5, 0, 0, 255);
            drawCross(img, predictedPoint, 5, 255, 0, 0);

	        for (unsigned int i = 0; i < listOfMousePoints.size() - 1; i++) {
		        line(img, listOfMousePoints[i], listOfMousePoints[i+1], Scalar(255, 255, 255), 1);
	        }
	        for (unsigned int i = 0; i < listOfEstimatedPoints.size() - 1; i++) {
		        line(img, listOfEstimatedPoints[i], listOfEstimatedPoints[i+1], Scalar(0, 0, 255), 1);
	        }
	        for (unsigned int i = 0; i < listOfPredictedPoints.size() - 1; i++) {
		        line(img, listOfPredictedPoints[i], listOfPredictedPoints[i+1], Scalar(255, 0, 0), 1);
	        }
	
            imshow("MouseTracker", img);

            if ((char)waitKey(1) > 0) break;
        }

        return ModuleState::OKAY;
    }
}


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


LaneDetector::LaneDetector(const int32_t &argc, char **argv) :
  ConferenceClientModule(argc, argv, "lanedetector") ,
  m_hasAttachedToSharedImageMemory(false) ,
  m_sharedImageMemory() ,
  m_image(NULL) ,
  m_cameraId(-1) ,
  m_debug(false) ,
  m_config() {
    m_config.th1 = 40;
    m_config.th2 = 10;
    m_config.hlTh = 10;
    m_config.hlMaxLineGap = 1;
    m_config.hlMaxLineLength = 8;
    m_config.caThVal = 200;
    m_config.caThMax = 200;
    m_config.caThTyp = 0;
    m_config.birdF = 600;
    m_config.birdDist = 300;
    m_config.birdAlpha = 9;
    m_config.birdBeta = 90;
    m_config.birdGamma = 90;
    m_config.dbEps = 25;
    m_config.dbMinPts = 3;
    m_config.dashMin = 25;
    m_config.dashMax = 50;
    m_config.dashWidth = 8;
    m_config.solidMin = 120;
    m_config.solidWidth = 30;
    m_config.pGain = 1;
    m_config.intGain = 50;
    m_config.derGain = 100;
    m_config.speed = 0;
  }

LaneDetector::~LaneDetector() {
}

void LaneDetector::setUp() {
	// This method will be call automatically _before_ running body().

  namedWindow("config",1);
  //createTrackbar("th1", "config", &m_config.th1, 250);
  //createTrackbar("th2", "config", &m_config.th2, 250);
  //createTrackbar("th", "config", &m_config.hlTh, 250);
  //createTrackbar("maxLineLength", "config", &m_config.hlMaxLineLength, 250);
  //createTrackbar("maxLineGap", "config", &m_config.hlMaxLineGap, 250);
  //createTrackbar("thValue", "config", &m_config.caThVal, 250);
  //createTrackbar("binMax", "config", &m_config.caThMax, 250);
  //createTrackbar("thType", "config", &m_config.caThTyp, 4);
  //cout << "\ndebug is on!\n\n" << endl;

  // BirdView
  //createTrackbar("f", "config", &m_config.birdF, 1500);
  //createTrackbar("dist", "config", &m_config.birdDist, 500);
  //createTrackbar("alpha", "config", &m_config.birdAlpha, 25);
  //createTrackbar("beta", "config", &m_config.birdBeta, 180);
  //createTrackbar("gamma", "config", &m_config.birdGamma, 360);
  // DBSCAN
  createTrackbar("eps", "config", &m_config.dbEps, 100);
  createTrackbar("minPts", "config", &m_config.dbMinPts, 100);
  createTrackbar("dashMin", "config", &m_config.dashMin, 100);
  createTrackbar("dashMax", "config", &m_config.dashMax, 200);
  createTrackbar("dashWidth", "config", &m_config.dashWidth, 25);
  createTrackbar("pGain", "config", &m_config.pGain, 100);
  createTrackbar("intGain", "config", &m_config.intGain, 100);
  createTrackbar("derGain", "config", &m_config.derGain, 100);
  createTrackbar("speed", "config", &m_config.speed, 100);
  // Create an OpenCV-window.
  //cvNamedWindow("WindowShowImage", CV_WINDOW_AUTOSIZE);
  //cvMoveWindow("WindowShowImage", 300, 100);
}

void LaneDetector::tearDown() {
	// This method will be call automatically _after_ return from body().
	if (m_image != NULL) {
		cvReleaseImage(&m_image);
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

static Scalar randomColor( RNG& rng )
{
  int icolor = (unsigned) rng;
  return Scalar( icolor&255, (icolor>>8)&255, (icolor>>16)&255 );
}

void LaneDetector::processImage() {
  TimeStamp currentTime_strt1;
  Mat dst, frame(m_image);
  dst = frame.clone();
  dst.setTo( Scalar(0,0,0));

  LineDetector road(frame, m_config, m_debug);
  carolocup::Lines lines = road.getLines();
  lines.pGain = m_config.pGain;
  lines.intGain = m_config.intGain;
  lines.derGain = m_config.derGain;
  lines.speed = m_config.speed;
  lines.width =  m_image->width;
  lines.height = m_image->height;

  LaneDetectionData data;
  data.setLaneDetectionData(lines);

	// Create a container from your data structure so that it can be transferred.
	// _remember_ the assigned ID (here 101): It must be used by the receiver to read the data successfully.
	// The ID must by from within this range: 0-127.
	Container con(Container::USER_DATA_1, data);

	// Send the data:
	getConference().send(con);

  // Create an instance of data structure for parking and set some values.

	TimeStamp currentTime_strt7;
	double timeStep_total = (currentTime_strt7.toMicroseconds() - currentTime_strt1.toMicroseconds()) / 1000.0;
	cout << "Total  " << timeStep_total << endl;

  if (m_debug) {
    Clusters* clusters = road.getClusters();
    RNG rng( 0xFFFFFFFF );

    for (vector<Cluster>::iterator it = clusters->begin(); it != clusters->end(); ++it) {
      Scalar color = randomColor(rng);
      for (vector<Point>::iterator it2 = it->begin(); it2 != it->end(); ++it2) {
        line( dst, *it2, *it2, color, 2, CV_AA);
      }
    }

    Line dashed = lines.dashedLine;
    Line solidRight = lines.rightLine;
    Line solidLeft = lines.leftLine;

    line( dst, Point(dashed[0], dashed[1]), Point(dashed[2], dashed[3]), Scalar(0,255,0), 3, CV_AA);
    line( dst, Point(solidRight[0], solidRight[1]), Point(solidRight[2], solidRight[3]), Scalar(255,0,0), 3, CV_AA);
    line( dst, Point(solidLeft[0], solidLeft[1]), Point(solidLeft[2], solidLeft[3]), Scalar(0,0,255), 3, CV_AA);

    imshow("birdView", dst);
    waitKey(50);
  }
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

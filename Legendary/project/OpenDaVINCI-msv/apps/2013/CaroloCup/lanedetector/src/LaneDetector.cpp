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
  m_debug(false) {
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
}

void LaneDetector::tearDown() {
	// This method will be call automatically _after_ return from body().

	if (m_image != NULL) {
		cvReleaseImage(&m_image);
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

  // REVIEW: Use LineDetector here to get the 3 lines.

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

	TimeStamp currentTime_strt7;
	double timeStep_total = (currentTime_strt7.toMicroseconds() - currentTime_strt1.toMicroseconds()) / (1000.0 * 1000.0);
	cout << "Total  "<< timeStep_total << endl;
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

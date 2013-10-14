/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <math.h>
#include <sys/time.h>
#include <opencv/cv.h>
#include <opencv/highgui.h>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/image/SharedImage.h"
#include "core/io/ContainerConference.h"
#include "core/wrapper/SharedMemoryFactory.h"

// Data structures from msv-data library:
#include "SteeringData.h"

#include "LaneDetector.h"

namespace msv {

  using namespace cv;
  using namespace std;
  using namespace core::base;
  using namespace core::data;
  using namespace core::data::image;

  LaneDetector::LaneDetector(const int32_t &argc, char **argv)
    : ConferenceClientModule(argc, argv, "lanedetector")
    , m_hasAttachedToSharedImageMemory(false)
    , m_sharedImageMemory()
    , m_image(NULL)
    , m_cameraId(-1)
    , m_debug(false)
  {}

  LaneDetector::~LaneDetector() {
  }

  // TODO: Check on the parameters!
  void LaneDetector::setUp() {
    // This method will be called automatically _before_ running body().
    if (m_debug) {
      // Create an OpenCV-window.
      cvNamedWindow("WindowShowImage", CV_WINDOW_AUTOSIZE);
      cvMoveWindow("WindowShowImage", 300, 100);
    }
  }


  void LaneDetector::tearDown() {
    // This method will be called automatically _after_ return from body().
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
      SharedImage si = c.getData<SharedImage>();

      // Check if we have already attached to the shared memory.
      if (!m_hasAttachedToSharedImageMemory) { // REVIEW: where is this thing set to true?
        m_sharedImageMemory =
          core::wrapper::SharedMemoryFactory::attachToSharedMemory(
              si.getName());
      }

      // Check if we could successfully attach to the shared memory.
      if (m_sharedImageMemory->isValid()) {
        // Lock the memory region to gain exclusive access. REMEMBER!!! DO NOT
        // FAIL WITHIN lock() / unlock(), otherwise, the image producing
        // process would fail.
        m_sharedImageMemory->lock();
        {
          const uint32_t numberOfChannels = 3;
          // For example, simply show the image.
          if (m_image == NULL) {
            m_image = cvCreateImage(
                cvSize(si.getWidth(), si.getHeight()), IPL_DEPTH_8U,
                numberOfChannels);
          }

          // Copying the image data is very expensive...
          if (m_image != NULL) {
            memcpy(
                // REVIEW && TODO: check this out, most probably you should use the SetData function.
                m_image->imageData, // Ignore this error (eclipse generated error), it seems to work anyways @Oscar
                m_sharedImageMemory->getSharedMemory(),
                si.getWidth() * si.getHeight() * numberOfChannels);
          }
        }

        // Release the memory region so that the image produce (i.e. the camera
        // for example) can provide the next raw image data.
        m_sharedImageMemory->unlock();

        // Mirror the image.
        // REVIEW: Why is the dst 0?
        cvFlip(m_image, 0, -1);

        retVal = true;
      }
    }
    return retVal;
  }


  void LaneDetector::processImage() {
    timeval t_start;
    // QUESTION: Why do we have to tell that gettimeofday and timeval have global scope explicitly? Check out if we really need it!
    ::gettimeofday(&t_start, 0);

    // Convert the image to a cv::Mat
    Mat lane(m_image), gray;

    // Convert the image to grayscale
    cvtColor(lane, gray, CV_BGR2GRAY);

    ::timeval t_end;
    ::gettimeofday(&t_end, (struct timezone*) 0);
    double elapsed = (t_end.tv_usec - t_start.tv_usec) / 1000.0F;
    cout << "time for lanefollower algorithm: " << elapsed << " ms\n";
    cout << "Steering Angle: " << angle << "\n";

    Container c(Container::USER_DATA_1, sd);

    getConference().send(c);
    if(m_debug) {
      imshow("image", gray);
    }
  }

  // Processing data:
  // Try to open the real camera first. If that fails, the
  // virtual camera images from camgen are used.
  ModuleState::MODULE_EXITCODE LaneDetector::body() {
    // Get configuration data.
    KeyValueConfiguration kv = getKeyValueConfiguration();
    m_cameraId = kv.getValue<int32_t>("lanedetector.camera_id");
    m_debug = kv.getValue<int32_t>("lanedetector.debug") == 1;

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

} // msv

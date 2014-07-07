/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

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

#include "RoadModel.h"
#include "LaneDetector.h"

namespace msv {

  using namespace cv;
  using namespace std;
  using namespace core::base;
  using namespace core::data;
  using namespace core::data::image;

  LaneDetector::LaneDetector(const int32_t &argc, char **argv) :
    ConferenceClientModule(argc, argv, "lanedetector"),
    m_hasAttachedToSharedImageMemory(false),
    m_sharedImageMemory(),
    m_image(NULL),
    m_cameraId(-1),
    m_debug(false),
    roadModel() { }

  LaneDetector::~LaneDetector() {
  }

  // REVIEW: Usable, but have a check on the parameters!
  void LaneDetector::setUp() {
    // This method will be call automatically _before_ running body().
    if (m_debug) {
      // Create an OpenCV-window.
      cvNamedWindow("WindowShowImage", CV_WINDOW_AUTOSIZE);
      cvMoveWindow("WindowShowImage", 300, 100);
    }
  }

  // REVIEW: Usable
  void LaneDetector::tearDown() {
    // This method will be call automatically _after_ return from body().
    if (m_image != NULL) {
      cvReleaseImage(&m_image);
    }

    if (m_debug) {
      cvDestroyWindow("WindowShowImage");
    }
  }

  // REVIEW: Usable
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
                // REVIEW: check this out, most probably you should use the SetData function.
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

  // You should start your work in this method.
  void LaneDetector::processImage() {
    //Example: Show the image.
    // if (m_debug) {
    // if (m_image != NULL) {
    // cvShowImage("WindowShowImage", m_image);
    // cvWaitKey(10);
    // }
    // }

  // REVIEW: Usable start
    timeval t_start;
    ::gettimeofday(&t_start, 0);
    // 1. Do something with the image m_image here, for example: find lane marking features, optimize quality, ...

    // Convert the image to a cv::Mat
    Mat lane(m_image), gray;

    // Convert the image to grayscale
    cvtColor(lane, gray, CV_BGR2GRAY);
    // REVIEW: Usable end

    // get values from configuration file, this is for integration debugging.
    int straight = getKeyValueConfiguration().getValue<int>("lanedetector.straight");
    int curved = getKeyValueConfiguration().getValue<int>("lanedetector.curved");
    int noline = getKeyValueConfiguration().getValue<int>("lanedetector.noline");

    cout << "straight: " << straight << "\tcurved: " << curved << "\tnoline: " << noline << "\n";



    // getting the angle from the roadModel input from camera
    double angle = roadModel.findSteeringAngle(gray, straight, curved, noline);
    ::timeval t_end;// REVIEW: Usable
    ::gettimeofday(&t_end, (struct timezone*) 0);// REVIEW: Usable
    double elapsed = (t_end.tv_usec - t_start.tv_usec) / 1000.0F;// REVIEW: Usable
    cout << "time for lanefollower algorithm: " << elapsed << " ms\n";
    cout << "Steering Angle: " << angle << "\n";

    SteeringData sd;
    sd.setSteering_angle(angle);
    sd.setIntersection(roadModel.findIntersection(gray));
    cvWaitKey(10); // REVIEW: use something more sophisticed waiting method

    // 2. Calculate desired steering commands from your image features to be processed by driver.

    // Here, you see an example of how to send the data structure SteeringData
    // to the ContainerConference. This data structure will be received by all
    // running components. In our example, it will be processed by Driver.


    // REVIEW: Usable to the end
    // Create container for finally sending the data.
    Container c(Container::USER_DATA_1, sd);
    // Send container.
    getConference().send(c);
    if(m_debug) {
      imshow("image", gray);
    }
  }

  // REVIEW: Usable to the end
  // This method will do the main data processing job.
  // Therefore, it tries to open the real camera first. If that fails, the
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
        // if (true == first_frame) {
        // roadModel.Initiate(m_image);
        // first_frame = false;
        // }
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

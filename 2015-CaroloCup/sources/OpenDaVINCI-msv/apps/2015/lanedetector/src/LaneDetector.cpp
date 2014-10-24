/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <opencv/cv.h>
#include <opencv/highgui.h>
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"

#include "core/macros.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/image/SharedImage.h"
#include "core/io/ContainerConference.h"
#include "core/wrapper/SharedMemoryFactory.h"

#include "tools/player/Player.h"

// Data structures from msv-data library:
#include "SteeringData.h"

#include "LaneDetector.h"

namespace msv {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::data::image;
    using namespace tools::player;
    using namespace cv;

    bool debug;
    Config cfg;
    // Global variables for camera functions
    int  avg_time = 0, num_msmnt;
    double width = 752, height = 480;

    LaneDetector::LaneDetector(const int32_t &argc, char **argv) :
    	ConferenceClientModule(argc, argv, "lanedetector"),
        m_hasAttachedToSharedImageMemory(false),
        m_sharedImageMemory(),
        m_image(NULL),
        m_cameraId(-1),
        m_debug(false),
		m_config(),
		m_frame(),
		m_frame_count(0)
	{
		m_config.th1 = 150;//83;
		m_config.th2 = 230;
		m_config.hlTh = THRESH_BINARY;
		m_config.XTimesYMin = 0;
		m_config.XTimesYMax = 30;
		m_config.maxY = 205;//195;
		m_config.maxArea = 4;
	}

    LaneDetector::~LaneDetector() {}

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
			    // Lock the memory region to gain exclusive access. REMEMBER!!! DO NOT FAIL WITHIN lock() / unlock(), otherwise, the image producing process would fail.
			    m_sharedImageMemory->lock();
			    {
				    const uint32_t numberOfChannels = 3;
				    // For example, simply show the image.
				    if (m_image == NULL) {
					    m_image = cvCreateImage(cvSize(si.getWidth(), si.getHeight()), IPL_DEPTH_8U, numberOfChannels);
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
			    //cvFlip(m_image, 0, -1);

			    retVal = true;
		    }
	    }
	    return retVal;
    }

    void drawLines(msv::Lines* lines, Mat* dst, int offset) {
        Line dashed = lines->dashedLine;
        Line solidRight = lines->rightLine;
        Line solidLeft = lines->leftLine;

        line( *dst, Point(dashed[0], dashed[1]+offset), Point(dashed[2], dashed[3]+offset), Scalar(0,255,0), 3, CV_AA);
        line( *dst, Point(solidRight[0], solidRight[1]+offset), Point(solidRight[2], solidRight[3]+offset), Scalar(255,0,0), 3, CV_AA);
        line( *dst, Point(solidLeft[0], solidLeft[1]+offset), Point(solidLeft[2], solidLeft[3]+offset), Scalar(0,0,255), 3, CV_AA);
        line( *dst, lines->goalLine.p1, lines->goalLine.p2, 255, 3, CV_AA);
        line( *dst, lines->currentLine.p1, lines->currentLine.p2, 0, 3, CV_AA);
    }

    // You should start your work in this method.
    void LaneDetector::processImage()
    {

		TimeStamp currentTime_strt1;

		debug = m_debug;
		cout << "Debug: " << debug << endl;
		cfg = m_config;

		//Mat neededPart = m_frame(
		//		cv::Rect(1, 2 * height / 16 - 1, width - 1, 10 * height / 16 - 1));

		IplImage *dst = cvCreateImage(cvGetSize(m_image), 8, 1);
		cvCvtColor(m_image, dst, CV_BGR2GRAY);
		Mat neededPart = cv::cvarrToMat(dst); // Fixed bug that prevented to use the shared image

		LineDetector road(neededPart, cfg, debug, 1);
		msv::Lines lines = road.getLines();
		if (&lines != NULL)
			cout << "We have lines for frame " <<m_frame_count << endl;
		LaneDetectionData data;
		data.setLaneDetectionData(lines);
		data.setFrameCount(m_frame_count);
		Container con(Container::USER_DATA_1, data);

		// Send the data:
		//cout << "Send..." << endl;
		getConference().send(con);

		TimeStamp currentTime_strt7;
		double timeStep_total = (currentTime_strt7.toMicroseconds()
				- currentTime_strt1.toMicroseconds()) / 1000.0;
		if (debug)
			cout << "Total  " << timeStep_total << "ms" << endl;
		if (avg_time == 0) {
			avg_time = timeStep_total;
			num_msmnt = 1;
		} else {
			avg_time = (avg_time * num_msmnt + timeStep_total) / (num_msmnt + 1);
			num_msmnt = (num_msmnt + 1) % 10;
		}
		if (debug) {
			cout << dec;
			cout << "avg_time: " << avg_time << "ms" << endl;
		}

		if (lines.goalLine.p1.x == 0 && lines.goalLine.p1.y == 0
				&& lines.goalLine.p2.x == 0 && lines.goalLine.p2.y == 0
				&& lines.currentLine.p2.x == 0 && lines.currentLine.p2.y == 0) {
			cout << "Nothing in..." << endl;
		} else {
			drawLines(&lines, &neededPart, 0);
		}

		if (debug) {
			cout << "VP [x, y] : [" << lines.goalLine.p1.x << ", "
					<< lines.goalLine.p1.y << "]" << endl;
			cout << "Goal [x, y] : [" << lines.goalLine.p2.x << ", "
					<< lines.goalLine.p2.y << "]" << endl;
			cout << "Position [x, y] : [" << lines.currentLine.p2.x << ", "
					<< lines.currentLine.p2.y << "]" << endl;

			imshow("Result", neededPart);
		}

		waitKey(20);

    }

    // This method will do the main data processing job.
    // Therefore, it tries to open the real camera first. If that fails, the virtual camera images from camgen are used.
    ModuleState::MODULE_EXITCODE LaneDetector::body() {
	    // Get configuration data.
	    KeyValueConfiguration kv = getKeyValueConfiguration();
	    m_cameraId = kv.getValue<int32_t> ("lanedetector.camera_id");
	    m_debug = kv.getValue<int32_t> ("lanedetector.debug") == 1;

		if (m_debug) {
			namedWindow("config", 1);

			//Thresholding
			createTrackbar("th1", "config", &m_config.th1, 250);

			//Dash properties
			createTrackbar("min times", "config", &m_config.XTimesYMin, 5);
			createTrackbar("max times", "config", &m_config.XTimesYMax, 40);
			createTrackbar("max y", "config", &m_config.maxY, 400);
			createTrackbar("max area", "config", &m_config.maxArea, 7);
		}

        Player *player = NULL;

/*
        // Lane-detector can also directly read the data from file. This might be interesting to inspect the algorithm step-wisely.
        core::io::URL url("file://recorder.rec");

        // Size of the memory buffer.
        const uint32_t MEMORY_SEGMENT_SIZE = kv.getValue<uint32_t>("global.buffer.memorySegmentSize");

        // Number of memory segments.
        const uint32_t NUMBER_OF_SEGMENTS = kv.getValue<uint32_t>("global.buffer.numberOfMemorySegments");

        // If AUTO_REWIND is true, the file will be played endlessly.
        const bool AUTO_REWIND = true;

        player = new Player(url, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS);
*/

        // "Working horse."
	    while (getModuleState() == ModuleState::RUNNING) {
		    bool has_next_frame = false;
		    // Use the shared memory image.
			Container c;
			if (player != NULL) {  // Read the next container from file.
				c = player->getNextContainerToBeSent();
			} else { // Get the most recent available container for a SHARED_IMAGE.
				c = getKeyValueDataStore().get(Container::SHARED_IMAGE);
			}

			if (c.getDataType() == Container::SHARED_IMAGE) {
				// Example for processing the received container.
				has_next_frame = readSharedImage(c);
			}

		    // Process the read image.
		    if (true == has_next_frame) {
		    	++m_frame_count;
			    processImage();
		    }

	    }


        OPENDAVINCI_CORE_DELETE_POINTER(player);

        waitKey(20);
	    return ModuleState::OKAY;
    }

} // msv


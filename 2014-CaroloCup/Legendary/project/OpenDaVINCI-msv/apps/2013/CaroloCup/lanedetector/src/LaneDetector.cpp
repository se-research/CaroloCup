/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <cstdlib>
#include <pthread.h>
#include <unistd.h>

#include <uEye.h>
#include <ueye.h>


#include "opencv2/opencv.hpp"

#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/image/SharedImage.h"
#include "core/io/ContainerConference.h"
#include "core/wrapper/SharedMemoryFactory.h"
#include "LaneDetectionData.h"

#include "tools/player/Player.h"

#include "LaneDetector.h"
#include <stdio.h>
#include <math.h>


#define NUM_THREADS  5;

namespace carolocup
{

using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::image;
using namespace cv;
using namespace tools::player;

bool debug;
Config cfg;
// Global variables for camera functions
HIDS hCam = 0;
char* ppcImgMem;
int pid;
int ret, avg_time = 0, num_msmnt;
double width = 752, height = 480;

LaneDetector::LaneDetector(const int32_t &argc, char **argv) :
    ConferenceClientModule(argc, argv, "lanedetector") ,
    m_hasAttachedToSharedImageMemory(false) ,
    m_sharedImageMemory() ,
    m_image(NULL) ,
    img(NULL) ,
    m_cameraId(-1) ,
    init(0),
    m_debug(false) ,
    m_config(),
    m_frame()
{
    m_config.th1 = 150;//83;
    m_config.th2 = 230;
    m_config.hlTh = THRESH_BINARY;
    m_config.XTimesYMin = 0;
    m_config.XTimesYMax = 30;
    m_config.maxY = 205;//195;
    m_config.maxArea = 4;
}

LaneDetector::~LaneDetector()
{
}

void LaneDetector::setUp()
{
    // This method will be call automatically _before_ running body().
}

void LaneDetector::tearDown()
{
    // This method will be call automatically _after_ return from body().
    if (m_image != NULL)
    {
        cvReleaseImage(&m_image);
    }
}

bool LaneDetector::readSharedImage(Container &c)
{
    bool retVal = false;

    if (c.getDataType() == Container::SHARED_IMAGE)
    {
        SharedImage si = c.getData<SharedImage> ();

        // Check if we have already attached to the shared memory.
        if (!m_hasAttachedToSharedImageMemory)
        {
            m_sharedImageMemory
                = core::wrapper::SharedMemoryFactory::attachToSharedMemory(
                      si.getName());
        }

        // Check if we could successfully attach to the shared memory.
        if (m_sharedImageMemory->isValid())
        {

            // Lock the memory region to gain exclusive access. REMEMBER!!! DO NOT
            // FAIL WITHIN lock() / unlock(), otherwise, the image producing process
            // would fail.
            m_sharedImageMemory->lock();
            {
                // Here, do something with the image. For example, we simply show the image.

                const uint32_t numberOfChannels = 3;
                // For example, simply show the image.
                if (m_image == NULL)
                {
                    m_image = cvCreateImage(cvSize(si.getWidth(),
                                                   si.getHeight()), IPL_DEPTH_8U, numberOfChannels);
                }

                // Copying the image data is very expensive...
                if (m_image != NULL)
                {
                    memcpy(m_image->imageData,
                           m_sharedImageMemory->getSharedMemory(),
                           si.getWidth() * si.getHeight() * numberOfChannels);
                }
            }

            // Release the memory region so that the image produce (i.e. the camera
            // for example) can provide the next raw image data.
            m_sharedImageMemory->unlock();

            // Mirror the image.
            cvFlip(m_image, 0, -1);

            retVal = true;
        }
    }
    return retVal;

}

void drawLines(carolocup::Lines* lines, Mat* dst, int offset) {
    Line dashed = lines->dashedLine;
    Line solidRight = lines->rightLine;
    Line solidLeft = lines->leftLine;

    line( *dst, Point(dashed[0], dashed[1]+offset), Point(dashed[2], dashed[3]+offset), Scalar(0,255,0), 3, CV_AA);
    line( *dst, Point(solidRight[0], solidRight[1]+offset), Point(solidRight[2], solidRight[3]+offset), Scalar(255,0,0), 3, CV_AA);
    line( *dst, Point(solidLeft[0], solidLeft[1]+offset), Point(solidLeft[2], solidLeft[3]+offset), Scalar(0,0,255), 3, CV_AA);
    line( *dst, lines->goalLine.p1, lines->goalLine.p2, 255, 3, CV_AA);
    line( *dst, lines->currentLine.p1, lines->currentLine.p2, 0, 3, CV_AA);
}

int __nsleep(const struct timespec *req, struct timespec *rem)
{
    struct timespec temp_rem;
    if(nanosleep(req,rem)==-1)
        __nsleep(rem,&temp_rem);
    else
        return 1;
}
 
int msleep(unsigned long milisec)
{
    struct timespec req={0},rem={0};
    time_t sec=(int)(milisec/1000);
    milisec=milisec-(sec*1000);
    req.tv_sec=sec;
    req.tv_nsec=milisec*1000000L;
    __nsleep(&req,&rem);
    return 1;
}

void LaneDetector::processImage()
{

    TimeStamp currentTime_strt1;

    debug = m_debug;
    cout << "Debug: " << debug << endl;
    cfg = m_config;

    Mat neededPart = m_frame(cv::Rect(1, 2*height/16-1, width-1, 10*height/16-1));
    LineDetector road(neededPart, cfg, debug, 1);
    carolocup::Lines lines = road.getLines();
    if(&lines != NULL) cout << "We have lines!" << endl;
    LaneDetectionData data;
    data.setLaneDetectionData(lines);
    Container con(Container::USER_DATA_1, data);

    // Send the data:
    //cout << "Send..." << endl;
    getConference().send(con);

    TimeStamp currentTime_strt7;
    double timeStep_total = (currentTime_strt7.toMicroseconds() - currentTime_strt1.toMicroseconds()) / 1000.0;
    if(debug) cout << "Total  " << timeStep_total << "ms" << endl;
    if(avg_time == 0) {
        avg_time = timeStep_total;
        num_msmnt = 1;
    } else {
        avg_time = (avg_time * num_msmnt + timeStep_total) / (num_msmnt + 1);
        num_msmnt = (num_msmnt + 1) % 10;
    }
    if(debug) {
        cout << dec;
        cout << "avg_time: " << avg_time << "ms" << endl;
    }

    if(lines.goalLine.p1.x == 0 && lines.goalLine.p1.y == 0 && lines.goalLine.p2.x == 0 && lines.goalLine.p2.y == 0 && lines.currentLine.p2.x == 0 && lines.currentLine.p2.y == 0) {
	cout << "Nothing in..." << endl;
    } else {
    	drawLines(&lines, &neededPart, 0);
    }

    if(debug) {
	    cout << "VP [x, y] : [" << lines.goalLine.p1.x << ", " << lines.goalLine.p1.y << "]" << endl;
	    cout << "Goal [x, y] : [" << lines.goalLine.p2.x << ", " << lines.goalLine.p2.y << "]" << endl;
	    cout << "Position [x, y] : [" << lines.currentLine.p2.x << ", " << lines.currentLine.p2.y << "]" << endl;

	    imshow("Result", neededPart);
    }

    waitKey(20);
}

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE LaneDetector::body()
{
    // Get configuration data.
    KeyValueConfiguration kv = getKeyValueConfiguration();
    m_cameraId = kv.getValue<int32_t> ("lanedetector.camera_id");
    m_debug = kv.getValue<int32_t> ("lanedetector.debug") == 1;

    if(m_debug) {
	    namedWindow("config",1);

	    //Thresholding
	    createTrackbar("th1", "config", &m_config.th1, 250);

	    //Dash properties
	    createTrackbar("min times", "config", &m_config.XTimesYMin, 5);
	    createTrackbar("max times", "config", &m_config.XTimesYMax, 40);
	    createTrackbar("max y", "config", &m_config.maxY , 400);
	    createTrackbar("max area", "config", &m_config.maxArea , 7);
    }

    bool isInitSuccess = init_camera();
    if(isInitSuccess)
    {
        while (getModuleState() == ModuleState::RUNNING)
        {
            char *newPointer;
            if(get_image(newPointer))
            {
                img = cvCreateImageHeader(cvSize(752,480),IPL_DEPTH_8U,1);
                img->imageData = (char*) newPointer;
                Mat rawImg(img, false);
                m_frame = rawImg.clone();
//Comment here and plug the recorder
                processImage();
            }
        }

        deinit_camera();
    }else{
    	while (getModuleState() == ModuleState::RUNNING) {
    	bool has_next_frame = false;
    	Container c;
    	Player *player = NULL;
			if (player != NULL) {
				// Read the next container from file.
				c = player->getNextContainerToBeSent();
			} else {
				// Get the most recent available container for a SHARED_IMAGE.
				c = getKeyValueDataStore().get(Container::SHARED_IMAGE);
			}

			if (c.getDataType() == Container::SHARED_IMAGE) {
				// Example for processing the received container.
				has_next_frame = readSharedImage(c);
			}

			 if (true == has_next_frame) {
				processImage();
			 }

		}

    }

    waitKey(20);
    return ModuleState::OKAY;

}

/* Initializes the uEye camera. If camera initialization is successful, it
 * returns true, otherwise returns false */
bool LaneDetector::init_camera()
{
    int nRet = is_InitCamera (&hCam, NULL);

    is_AllocImageMem(hCam, width, height, 1 ,&ppcImgMem, &pid);
    is_SetImageMem(hCam, ppcImgMem, pid);
    is_SetDisplayMode (hCam, IS_SET_DM_DIB);
    is_SetColorMode (hCam, IS_CM_MONO8);
    int pnCol , pnColMode;
    is_GetColorDepth(hCam, &pnCol , &pnColMode);

    is_CaptureVideo(hCam, IS_WAIT);

    if (nRet != IS_SUCCESS)
    {
        if (nRet == IS_STARTER_FW_UPLOAD_NEEDED)
        {
            hCam = hCam | IS_ALLOW_STARTER_FW_UPLOAD;
            nRet = is_InitCamera (&hCam, NULL);
        }
        cout << "camera failed to initialize " << endl;
        return false;
    }
    else
        return true;
}

/* Deinitializes the uEye camera. Returns true if deinitialization is
* successful, otherwise returns false */
bool LaneDetector::deinit_camera()
{

    // Free the allocated memory
    is_FreeImageMem(hCam, ppcImgMem, pid);

    // Try to deinitialize camera. If successful, return true, otherwise return
    // false
    if(is_ExitCamera(hCam) == IS_SUCCESS)
        return true;
    else
        return false;
}

/* Retrieves an image from the camera, in the form of a signed char pointer. Signed
 * char is meant for IPL images. If the data is to be used with a Mat, the char
 * should be cast into an unsigned char i.e. (uchar* ).
 *
 * Arguments: char* to be passed in by reference, which will contain the
 * image. I.e. char* imgPointer; get_image(imgPointer);
 * Returns a boolean, true if image is retrieved, false if it fails.
*/
bool LaneDetector::get_image(char*& img)
{
    void *pMemVoid; //pointer to where the image is stored

    // Takes an image from the camera. If successful, returns true, otherwise
    // returns false
    if (is_GetImageMem(hCam, &pMemVoid) == IS_SUCCESS)
    {

        img = (char*) pMemVoid;
        return true;
    }
    else
        return false;
}

} // carolocup

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

///////////////////////////////////////////////////////////////////////////////
/*The three functions below are function declarations for the Thread functions*/
void  *functionBottom(void *argument);
void  *functionMiddle(void *argument);
void  *functionTop(void *argument);

/*These are three Mat pointers, one for each segment of the image*/
Mat *getFirstPointer;
Mat *getSecondPointer;
Mat *getThirdPointer;

/* Mutex variables that would be shared between threads */
pthread_mutex_t running_mutex_bottom = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t running_mutex_middle = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t running_mutex_top = PTHREAD_MUTEX_INITIALIZER;

carolocup::Lines linesTop, linesMiddle, linesBottom;
volatile bool topDone = false, middleDone = false, bottomDone = false;
bool debug;
Config cfg;
// Global variables for camera functions
HIDS hCam = 0;
char* ppcImgMem;
int pid;
pthread_t t1, t2; // t3 ;
int ret, avg_time = 0, num_msmnt;
double w, h;
///////////////////////////////////////////////////////////////////////////////

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
    m_config.th1 = 250;
    m_config.th2 = 250;
    m_config.hlTh = 0;
    m_config.caThVal = 300;
    m_config.caThMax = 100;
    m_config.caThTyp = 3;
    m_config.pGain = 10;
    m_config.intGain = 19;
    m_config.derGain = 23;
    m_config.houghMinAngle = 20;
    m_config.houghMaxAngle = 160;
    m_config.houghStartVal = 25;
    m_config.houghMaxLines = 40;
}

LaneDetector::~LaneDetector()
{
}

void LaneDetector::setUp()
{
    // This method will be call automatically _before_ running body().

    namedWindow("config",1);
    /*createTrackbar("pGain", "config", &m_config.pGain, 100);
    createTrackbar("intGain", "config", &m_config.intGain, 100);
    createTrackbar("derGain", "config", &m_config.derGain, 100);
    createTrackbar("speed", "config", &m_config.speed, 100);*/

    createTrackbar("th1", "config", &m_config.th1, 250);
    createTrackbar("th2", "config", &m_config.th2, 250);
    createTrackbar("thType", "config", &m_config.hlTh, 4);
    //createTrackbar("maxLineLength", "config", &m_config.hlMaxLineLength, 250);
    //createTrackbar("maxLineGap", "config", &m_config.hlMaxLineGap, 250);
    createTrackbar("canTh", "config", &m_config.caThVal, 500);
    createTrackbar("canThWithRatio", "config", &m_config.caThMax, 500);
    createTrackbar("canNoOfKernels", "config", &m_config.caThTyp, 10);
    createTrackbar("houghStart", "config", &m_config.houghStartVal, 100);
    createTrackbar("houghMaxLines", "config", &m_config.houghMaxLines, 500);

    // BirdView
    //createTrackbar("f", "config", &m_config.birdF, 1500);
    //createTrackbar("dist", "config", &m_config.birdDist, 500);
    //createTrackbar("alpha", "config", &m_config.birdAlpha, 25);
    //createTrackbar("beta", "config", &m_config.birdBeta, 180);
    //createTrackbar("gamma", "config", &m_config.birdGamma, 360);
    // DBSCAN
    /*createTrackbar("eps", "config", &m_config.dbEps, 100);
    createTrackbar("minPts", "config", &m_config.dbMinPts, 100);
    createTrackbar("dashMin", "config", &m_config.dashMin, 100);
    createTrackbar("dashMax", "config", &m_config.dashMax, 200);
    createTrackbar("dashWidth", "config", &m_config.dashWidth, 25);
    createTrackbar("solidMin", "config", &m_config.solidMin, 150);
    createTrackbar("solidWidth", "config", &m_config.solidWidth, 15);*/
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

}

void drawLines(carolocup::Lines* lines, Mat* dst, int offset) {
    Line dashed = lines->dashedLine;
    Line solidRight = lines->rightLine;
    Line solidLeft = lines->leftLine;

    line( *dst, Point(dashed[0], dashed[1]+offset), Point(dashed[2], dashed[3]+offset), Scalar(0,255,0), 3, CV_AA);
    line( *dst, Point(solidRight[0], solidRight[1]+offset), Point(solidRight[2], solidRight[3]+offset), Scalar(255,0,0), 3, CV_AA);
    line( *dst, Point(solidLeft[0], solidLeft[1]+offset), Point(solidLeft[2], solidLeft[3]+offset), Scalar(0,0,255), 3, CV_AA);
    /*if(lines->stopLineHeight != -1) {
        line( *dst, Point(0, lines->stopLineHeight), Point(640, lines->stopLineHeight), Scalar(0,255,0), 3, CV_AA);
    }
    if(lines->startLineHeight != -1) {
        line( *dst, Point(0, lines->startLineHeight), Point(640, lines->startLineHeight), Scalar(255,0,0), 3, CV_AA);
    }*/
}

/////////////////////////////////////////////////////START//////////////////////////////////////////////////////////////////////////////////
/*The three functions below are the functions that is run by each of the threads
You can do stuff with the image segment in each function. Because of issues with
	 X server and Multi-threads I can't use"imshow within the Thread functions",
	If you want to show the images, show them outside the function somewhere in
	the processImage() function*/

void *functionBottom(void *argument)
{
    cout << "\nRunning this from Thread_Bottom" << endl;
    int i = 0;
    argument = (void *) i;
    cout << argument << endl;
    LineDetector road(*getFirstPointer, cfg, debug, 1);
    linesBottom = road.getLines();
    //linesBottom.stopLineHeight = road.detectStopLine(10);
    //linesBottom.startLineHeight = road.detectStartLine(10);
    pthread_mutex_lock(&running_mutex_bottom);
    bottomDone = true;
    pthread_mutex_unlock(&running_mutex_bottom);
    cout << "Bottom end" << endl;
    return 0;
}


void *functionMiddle(void *argument)
{
    cout << "\nRunning this from Thread_Middle" << endl;
    int i = 0;
    argument = (void *) i;
    cout << argument;
    LineDetector road(*getSecondPointer, cfg, debug, 2);
    linesMiddle = road.getLines();
    //linesMiddle.stopLineHeight = road.detectStopLine(10);
    //linesMiddle.startLineHeight = road.detectStartLine(10);
    pthread_mutex_lock(&running_mutex_middle);
    middleDone = true;
    pthread_mutex_unlock(&running_mutex_middle);
    cout << "Middle end" << endl;
    return 0;
}

void *functionTop(void *argument)
{
    cout << "\nRunning this from Thread_Top" << endl;
    int i = 0;
    argument = (void *) i;
    cout << argument;

    LineDetector road(*getThirdPointer, cfg, debug, 3);
    linesTop = road.getLines();
    //linesTop.stopLineHeight = road.detectStopLine(10);
    //linesTop.startLineHeight = road.detectStartLine(10);
    pthread_mutex_lock(&running_mutex_top);
    topDone = true;
    pthread_mutex_unlock(&running_mutex_top);
    cout << "Top end" << endl;
    return 0;
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

//////////////////////////////////////////////////////END///////////////////////////////////////////////////////////////////////////////////

carolocup::Lines mergeLinesData()
{
    linesBottom.dashedLine[1] = linesBottom.dashedLine[1] != 0 ? linesBottom.dashedLine[1] + h/2 : 0;
    linesBottom.dashedLine[3] = linesBottom.dashedLine[3] != 0 ? linesBottom.dashedLine[3] + h/2 : 0;
    linesBottom.leftLine[1] = linesBottom.leftLine[1] != 0 ? linesBottom.leftLine[1] + h/2 : 0;
    linesBottom.leftLine[3] = linesBottom.leftLine[3] != 0 ? linesBottom.leftLine[3] + h/2 : 0;
    linesBottom.rightLine[1] = linesBottom.rightLine[1] != 0 ? linesBottom.rightLine[1] + h/2 : 0;
    linesBottom.rightLine[3] = linesBottom.rightLine[3] != 0 ? linesBottom.rightLine[3] + h/2 : 0;
    linesBottom.width = w;
    linesBottom.height =  h;
    linesMiddle.dashedLine[1] = linesMiddle.dashedLine[1] != 0 ? linesMiddle.dashedLine[1] + 3*h/8 : 0;
    linesMiddle.dashedLine[3] = linesMiddle.dashedLine[3] != 0 ? linesMiddle.dashedLine[3] + 3*h/8 : 0;
    linesMiddle.leftLine[1] = linesMiddle.leftLine[1] != 0 ? linesMiddle.leftLine[1] + 3*h/8 : 0;
    linesMiddle.leftLine[3] = linesMiddle.leftLine[3] != 0 ? linesMiddle.leftLine[3] + 3*h/8: 0;
    linesMiddle.rightLine[1] = linesMiddle.rightLine[1] != 0 ? linesMiddle.rightLine[1] + 3*h/8 : 0;
    linesMiddle.rightLine[3] = linesMiddle.rightLine[3] != 0 ? linesMiddle.rightLine[3] + 3*h/8 : 0;
    linesMiddle.width = w;
    linesMiddle.height =  h;
    return linesBottom;
}

/*
matrix<double,4,1> getCurvePolynom() {
    
}
*/

void LaneDetector::processImage()
{

    /*Mat pickMat(m_image,false);
     imshow("Source Image", pickMat);*/

    TimeStamp currentTime_strt1;
    //Mat dst;
    //cout<<"Showing Input Frame............"<<endl;

    //dst = m_frame.clone();

    //cout<<"Cloning............"<<endl;
    //dst.setTo( Scalar(0,0,0));
    debug = m_debug;
    cfg = m_config;
//////////////////////////////////////////////////////START//////////////////////////////////////////////////////////////////////////////////
    /*Each of Mat Images below represents a segment of the image
    and their respective addresses are assigned to each of the pointers below*/
    
    cout<<"Spliting............"<<endl;
    w = m_frame.size().width;
    h = m_frame.size().height;
    Mat getFirst = m_frame(cv::Rect(1, h/2-1, w-1, h/2-1));
    Mat getSecond  = m_frame(cv::Rect(1,3*h/8-1, w-1, h/8-1));
    //Mat getThird = m_frame(cv::Rect(1,1,w-1,h/4-1));


    getFirstPointer = &getFirst;
    getSecondPointer = &getSecond;
    //getThirdPointer = &getThird;

    cout<<"Creating Threads............"<<endl;
    
    /*Threes POSIX threads are created below and each of the threads are assigned to run each of the three functions*/
    cout << "Threads started!" << endl;
    ret = pthread_create(&t1, NULL, functionBottom,NULL);
    if(ret != 0) {
        cout << "Unable to create p1" << endl;
        bottomDone = true;
    }
    pthread_create(&t2, NULL, functionMiddle,NULL);
    if(ret != 0) {
        cout << "Unable to create p2" << endl;
        middleDone = true;
    }
    //middleDone = true;
    //topDone = true;
    while(true){
        bool ok = false;
        pthread_mutex_lock(&running_mutex_bottom);
        if(bottomDone) ok = true;
        pthread_mutex_unlock(&running_mutex_bottom);
        pthread_mutex_lock(&running_mutex_middle);
        if(middleDone) ok = ok && true;
        pthread_mutex_unlock(&running_mutex_middle);
        msleep(1);
        if(ok) break;
    }
    pthread_join(t1,NULL);
    pthread_join(t2,NULL);
    bottomDone = false;
    middleDone = false;
    cout<<"Threads Done............"<<endl;
    carolocup::Lines lines = mergeLinesData();
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
    cout << "Total  " << timeStep_total << "ms" << endl;
    if(avg_time == 0) {
        avg_time = timeStep_total;
        num_msmnt = 1;
    } else {
        avg_time = (avg_time * num_msmnt + timeStep_total) / (num_msmnt + 1);
        num_msmnt = (num_msmnt + 1) % 10;
    }
    cout << dec;
    cout << "avg_time: " << avg_time << "ms" << endl;

    /*if (m_debug) {
    imshow("output", dst);
    }*/
    /*As I stated earlier, because of restrictions with X SERVER and Multi-threads
    It will be difficult and messy trying to show the frames withing the thread functions
           So you can show them outside like I have done here*/
    //imshow("Input Image", m_frame);
    //LineDetector roadB(getFirst, cfg, m_debug, 1);
    //linesBottom = roadB.getLines();
    //cfg.houghMinAngle = 5;
    //cfg.houghMaxAngle = 175;
    /*LineDetector roadM(getSecond, cfg, m_debug, 2);
    linesMiddle = roadM.getLines();
    lines.width =  w;
    lines.height = h;
    lines.pGain = m_config.pGain;
    lines.intGain = m_config.intGain;
    lines.derGain = m_config.derGain;
    lines.speed = m_config.speed;
    lines.stopLineHeight = road.detectStopLine(10);
    lines.startLineHeight = road.detectStartLine(10);*/
    drawLines(&linesBottom, &m_frame, 0);
    drawLines(&linesMiddle, &m_frame, 0);
    //imshow("First Frame", *getFirstPointer);
    //imshow("Second Frame", *getSecondPointer);
    //imshow("Third Frame", *getThirdPointer);
    imshow("Output", m_frame);

//////////////////////////////////////////////////////END////////////////////////////////////////////////////////////////////////////////
    waitKey(20);
    //}
}

// This method will do the main data processing job.
ModuleState::MODULE_EXITCODE LaneDetector::body()
{
    // Get configuration data.
    KeyValueConfiguration kv = getKeyValueConfiguration();
    m_cameraId = kv.getValue<int32_t> ("lanedetector.camera_id");
    m_debug = kv.getValue<int32_t> ("lanedetector.debug") == 1;
////////////////////////////////////////////////////////////////////////////////
    /*	HIDS m_hCam=1;
    	int m_nBitsPerPixel=8;
    	int m_nColorMode= 6;
    	int m_nSizeX = 758;
    	int m_nSizeY = 480;
    	char* m_pcImageMemory=NULL;
    	int m_lMemoryId=0;

    	//double dEnable = 1;
    	double nominal = 128;


    	//int ret = is_SetAutoParameter(m_hCam,IS_SET_ENABLE_AUTO_GAIN,&dEnable,0);
    	is_SetAutoParameter(m_hCam,IS_SET_AUTO_REFERENCE,&nominal,0);

    	INT nRet = is_InitCamera(&m_hCam,NULL);
    	if(nRet != IS_SUCCESS)
    	{
    	    cout << "Error: Camera not found" << endl;
    	    abort();
    	}
    	if(nRet == IS_SUCCESS)
    	{

    	    is_GetColorDepth(m_hCam,&m_nBitsPerPixel,&m_nColorMode);
    	    if(is_SetColorMode(m_hCam,m_nColorMode) != IS_SUCCESS)
    		{   cout << "Error: Set ColorMode" << endl; }
    	}

    	INT *pnSizeX = &m_nSizeX;
    	INT *pnSizeY = &m_nSizeY;
    	INT nAOISupported = 0;

    	BOOL bAOISupported = TRUE;
    	if (is_ImageFormat (m_hCam,IMGFRMT_CMD_GET_ARBITRARY_AOI_SUPPORTED,(void*)&nAOISupported,sizeof(nAOISupported))  == IS_SUCCESS)
    	{
    	    bAOISupported = (nAOISupported != 0);
    	}
    	if (bAOISupported)
    	{
    	    SENSORINFO sInfo;
    	    is_GetSensorInfo(m_hCam,&sInfo);
    	    *pnSizeX = sInfo.nMaxWidth;
    	    *pnSizeY = sInfo.nMaxHeight;

    	}
    	else
    	{
    	    //IS_RECT rectAOI;
    	    //rectAOI.s32X = 0;
    	    //rectAOI.s32Y = 0;
    	    //rectAOI.s32Width = m_nSizeX;
    	    //rectAOI.s32Height = m_nSizeY;
    	   // is_AOI(m_hCam,IS_AOI_IMAGE_SET_AOI,(void*)&rectAOI,sizeof(rectAOI));
    	}
    	nRet = is_AllocImageMem(m_hCam,m_nSizeX,m_nSizeY,m_nBitsPerPixel,&m_pcImageMemory,&m_lMemoryId);
    	nRet = nRet && is_SetImageMem(m_hCam, m_pcImageMemory, m_lMemoryId);

    	nRet = nRet && is_SetDisplayMode(m_hCam,IS_SET_DM_DIB);
    	if(nRet != IS_SUCCESS)
    	{
    	    cout << "Error: is_SetDisplayMode" << endl;
    	}*/

////////////////////////////////////////////////////////////////////////////////
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
                //m_frame = rawImg(cv::Rect(1,1,639, 479));
                //m_frame = rawImg(cv::Rect(1,239,751,239));
                //cvNamedWindow("image", CV_WINDOW_AUTOSIZE);
                //cvShowImage("image", img);
                //cout<<"Processing"<<endl;
                processImage();
            }
            /*
                    if(m_hCam != 0)
            	{

                    //is_FreezeVideo(m_hCam,IS_WAIT);
            	is_CaptureVideo(m_hCam,IS_WAIT);
            	is_RenderBitmap(m_hCam,m_lMemoryId,NULL,IS_RENDER_FIT_TO_WINDOW);

                    void *newPointer;
                    if(is_GetImageMem(m_hCam, (void**)newPointer) == IS_SUCCESS){
            	    img = cvCreateImageHeader(cvSize(758,480),IPL_DEPTH_8U,1);
            	    img->imageData = (char*) newPointer;
                        cvNamedWindow("image", CV_WINDOW_AUTOSIZE);
            	    cvShowImage("image", img);
                        cout<<"Processing"<<endl;

                        Mat Maa(img, false);
                        m_frame = Maa.clone();
                        cout<<"In progress"<<endl;
                        //processImage();
                        cout<<"Processing Done"<<endl;
                    }
                   // cv::imshow("Mat", Maa);
                   //is_FreeImageMem(m_hCam,m_pcImageMemory,m_lMemoryId);
                   // is_ExitCamera(m_hCam);
                   cvWaitKey(99);*/
        }
    }
    deinit_camera();
    waitKey(20);
    return ModuleState::OKAY;

}

/* Initializes the uEye camera. If camera initialization is successful, it
 * returns true, otherwise returns false */
bool LaneDetector::init_camera()
{
    int nRet = is_InitCamera (&hCam, NULL);

    is_AllocImageMem(hCam,752, 480, 1 ,&ppcImgMem, &pid);
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

/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <ctime>
#include <opencv/cv.h>
#include <opencv/highgui.h>
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"

#include "opendavinci/odcore/opendavinci.h"
#include "opendavinci/odcore/base/KeyValueConfiguration.h"
#include "opendavinci/odcore/data/Container.h"
#include "opendavinci/odcore/io/conference/ContainerConference.h"
#include "opendavinci/odcore/wrapper/SharedMemoryFactory.h"
#include "opendavinci/odtools/player/Player.h"

#include "opendavinci/GeneratedHeaders_OpenDaVINCI.h"
#include "automotivedata/GeneratedHeaders_AutomotiveData.h"


#include "LaneDetector.h"



namespace msv
{

using namespace std;
using namespace odcore::base;
using namespace odcore::data;
using namespace odcore::data::image;
using namespace odtools::player;
using namespace cv;
using namespace automotive::miniature;

int previousThresh=48;
bool debug;
Config cfg;
// Global variables for camera functions
int  avg_time = 0, num_msmnt;
double width = 752, height = 480;
Mat img;

LaneDetector::LaneDetector(const int32_t &argc, char **argv) :
    TimeTriggeredConferenceClientModule(argc, argv, "lanedetector"),
    m_hasAttachedToSharedImageMemory(false),
    m_sharedImageMemory(),
    m_cameraId(-1),
    m_debug(false),
    m_config(),
    m_frame(),
    m_frame_count(0)
{
    m_config.th1 = 70;//143; //150;//83;
    m_config.th2 = 230;
    m_config.hlTh = THRESH_BINARY;
    m_config.XTimesYMin = 2;
    m_config.XTimesYMax = 20;
    m_config.maxY = 235; //205;//195;
    m_config.maxArea = 4;
}

LaneDetector::~LaneDetector() {}

void LaneDetector::setUp()
{
	cout << "Debug is " << m_debug << std::endl;
    // This method will be call automatically _before_ running body().
}

    void LaneDetector::tearDown()
{
    // This method will be call automatically _after_ return from body().
    if (m_debug)
        {
            cvDestroyWindow("WindowShowImage");
            cvDestroyWindow("config");
        }

    if (m_hasAttachedToSharedImageMemory)
        {
            m_sharedImageMemory.reset();
        }
}

bool LaneDetector::readSharedImage(Container &c)
{
    bool retVal = false;
    IplImage *image(NULL);

    if (c.getDataType() == SharedImage::ID())
        {
            SharedImage si = c.getData<SharedImage> ();

            // Check if we have already attached to the shared memory.
            if (!m_hasAttachedToSharedImageMemory)
                {
                    m_sharedImageMemory
                        = odcore::wrapper::SharedMemoryFactory::attachToSharedMemory(
                              si.getName());
                }

            // Check if we could successfully attach to the shared memory.
            if (m_sharedImageMemory->isValid())
                {
                    // Lock the memory region to gain exclusive access. REMEMBER!!! DO NOT FAIL WITHIN lock() / unlock(), otherwise, the image producing process would fail.
                    m_sharedImageMemory->lock();
                    {
                        const uint32_t numberOfChannels = si.getBytesPerPixel();
                        width = si.getWidth();
                        height = si.getHeight();

                        // For example, simply show the image.
                        if (image == NULL)
                            {
                                image = cvCreateImageHeader(cvSize(width, height), IPL_DEPTH_8U, numberOfChannels);
                                //m_image = cvCreateImage(cvSize(width, height), IPL_DEPTH_8U, numberOfChannels);
                            }

                        // Copying the image data is very expensive...
                        if (image != NULL)
                            {
                                image->imageData = (char *)m_sharedImageMemory->getSharedMemory();
                                Mat rawImg(image, true);

                                if (numberOfChannels == 1)
                                    {
                                        m_frame = rawImg;
                                    }
                                else
                                    {
                                        //We need to convert this to gray scale
                                        Mat bwImage;
                                        cv::cvtColor(rawImg, bwImage, CV_RGB2GRAY);
                                        m_frame = bwImage;
                                    }

                                //memcpy(m_image->imageData,
                                //   (char *)m_sharedImageMemory->getSharedMemory(),
                                //  width * height * numberOfChannels);
                                cvReleaseImage(&image);
                            }
                    }

                    // Release the memory region so that the image produce (i.e. the camera for example) can provide the next raw image data.
                    m_sharedImageMemory->unlock();
                    if(m_debug)
                    	cout << "Memory unlock" << endl;
                    // Mirror the image.
                    //cvFlip(m_image, 0, -1);

                    retVal = true;
                }
        }
    return retVal;
}

void drawLines(msv::Lines *lines, Mat *dst, int offset)
{
    Line dashed = lines->dashedLine;
    Line solidRight = lines->rightLine;
    Line solidLeft = lines->leftLine;

    line( *dst, Point(dashed[0], dashed[1] + offset), Point(dashed[2], dashed[3] + offset), Scalar(0, 255, 0), 3, CV_AA);
    line( *dst, Point(solidRight[0], solidRight[1] + offset), Point(solidRight[2], solidRight[3] + offset), Scalar(255, 0, 0), 3, CV_AA);
    line( *dst, Point(solidLeft[0], solidLeft[1] + offset), Point(solidLeft[2], solidLeft[3] + offset), Scalar(0, 0, 255), 3, CV_AA);
    line( *dst, lines->goalLine.p1, lines->goalLine.p2, 255, 3, CV_AA);
    line( *dst, lines->goalLineLeft.p1, lines->goalLineLeft.p2, 124,3,CV_AA);
    line( *dst, lines->currentLine.p1, lines->currentLine.p2, 0, 3, CV_AA);
}
int LaneDetector::getDynamicThresh(int lux)
{
  int baseThresh=getKeyValueConfiguration().getValue<uint32_t>("lanedetector.threshBaseParameter");
  int minIntervalValue[]={11,15,17,20,23,26,29,32},maxIntervalValue[]={16,18,21,24,27,31,35,40};
  int foundIndex[3],thresh[]={baseThresh+2,baseThresh+7,baseThresh+12,baseThresh+17,baseThresh+22,baseThresh+27,baseThresh+32};
  if(lux<minIntervalValue[0])
    {
      return baseThresh;
    }
  if(lux>maxIntervalValue[6]){
      return baseThresh+42;
  }
  int cnt=0;
  for(int i=0;i<7;i++)
    {
      if(lux>=minIntervalValue[i] && lux<=maxIntervalValue[i])
	{
	  foundIndex[cnt++]=i;
	}
    }
      for(int j=0;j<cnt;j++)
	{
	  if(previousThresh==thresh[foundIndex[j]])
	    {
	      return thresh[foundIndex[j]];
	    }
	}
  return thresh[foundIndex[0]];
}

// You should start your work in this method.
void LaneDetector::processImage() {
    SensorBoardData sdb;
    Container conUserData0 = getKeyValueDataStore ().get (SensorBoardData::ID());
    sdb = conUserData0.getData<SensorBoardData> ();
    //int lux = sdb.getDistance(7);
    int lux=-2;
    if(sdb.containsKey_MapOfDistances(7))
        lux=sdb.getValueForKey_MapOfDistances(7);

    if(m_debug)
    	cout<<"LUX::"<<lux<<endl;
    TimeStamp currentTime_strt1;

    previousThresh=m_config.th1;
//    m_config.th1 =  150;
    m_config.currentDistance =(sdb.containsKey_MapOfDistances(6)) ? (int) sdb.getValueForKey_MapOfDistances(6) : 0;
//    m_config.th1 =  getDynamicThresh(lux);

    if(m_debug)
    	cout<<"Thresh:"<<m_config.th1<<endl;
    cfg = m_config;

    Mat neededPart = m_frame(cv::Rect(0, 2 * height / 16 + 30, width - 1, 10 * height / 16 - 40));

    LineDetector road(neededPart, cfg, m_debug, 1);

    if (m_debug)
    	showResult(road, neededPart);

    // Start fix. This code deactivates the old estimateLines and calculatesGoalLine()
    //msv::Lines lines = road.getLines();
//    msv::Lines lines = *(new Lines());
        msv::Lines lines;
    // End fix.

    msv::LaneDetectorDataToDriver dataToDriver = *(road.getDriverData());
    dataToDriver.setRoadState(road.getRoadState());
    dataToDriver.setConfidence(road.getConfidenceLevel());

    lines.setCurrentLine(dataToDriver.currentLine);

    if (m_debug && &lines != NULL)
        cout << "We have lines for frame " << m_frame_count << endl;
    LaneDetectionData data;
    data.setLaneDetectionData(lines, dataToDriver);
    data.setFrameCount(m_frame_count);
    Container con(data);

    // Send the data:
    //cout << "Send..." << endl;
    getConference().send(con);

    TimeStamp currentTime_strt7;
    double timeStep_total = (currentTime_strt7.toMicroseconds()
                             - currentTime_strt1.toMicroseconds()) / 1000.0;
    if (m_debug)
        cout << "Total  " << timeStep_total << "ms" << endl;
    if (avg_time == 0)
        {
            avg_time = timeStep_total;
            num_msmnt = 1;
        }
    else
        {
            avg_time = (avg_time * num_msmnt + timeStep_total) / (num_msmnt + 1);
            num_msmnt = (num_msmnt + 1) % 10;
        }
    if (m_debug)
        {
            cout << dec;
            cout << "avg_time: " << avg_time << "ms" << endl;
        }

    // The following lines are about getting data from Lines => legacy stuff

    // if (lines.goalLine.p1.x == 0 && lines.goalLine.p1.y == 0
    //         && lines.goalLine.p2.x == 0 && lines.goalLine.p2.y == 0
    //         && lines.currentLine.p2.x == 0 && lines.currentLine.p2.y == 0)
    //     {
    //         cout << "Nothing in..." << endl;
    //     }
    // else
    //     {
    //         drawLines(&lines, &neededPart, 0);
    //     }

    // if (debug)
    //     {
    //         cout << "VP [x, y] : [" << lines.goalLine.p1.x << ", "
    //              << lines.goalLine.p1.y << "]" << endl;
    //         cout << "Goal [x, y] : [" << lines.goalLine.p2.x << ", "
    //              << lines.goalLine.p2.y << "]" << endl;
    //         cout << "GoalLineLeft p1:"<<lines.goalLineLeft.p1.x<<","<<lines.goalLineLeft.p1.y<<endl;
    //         cout << "GoalLineLeft p2:"<<lines.goalLineLeft.p2.x<<","<<lines.goalLineLeft.p2.y<<endl;
    //         cout << "Position [x, y] : [" << lines.currentLine.p2.x << ", "
    //              << lines.currentLine.p2.y << "]" << endl;

    //         imshow("Result", neededPart);
    //     }

    neededPart.release();
    m_frame.release();
    waitKey(20);

}

// This method will do the main data processing job.
// Therefore, it tries to open the real camera first. If that fails, the virtual camera images from camgen are used.
odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode LaneDetector::body()
{


    // Get configuration data.
    KeyValueConfiguration kv = getKeyValueConfiguration();

    m_cameraId = kv.getValue<int32_t> ("lanedetector.camera_id");
    m_debug = kv.getValue<int32_t> ("lanedetector.debug") == 1;

    if (m_debug)
        {
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
            odcore::io::URL url("file://recorder.rec");

            // Size of the memory buffer.
            const uint32_t MEMORY_SEGMENT_SIZE = kv.getValue<uint32_t>("global.buffer.memorySegmentSize");

            // Number of memory segments.
            const uint32_t NUMBER_OF_SEGMENTS = kv.getValue<uint32_t>("global.buffer.numberOfMemorySegments");

            // If AUTO_REWIND is true, the file will be played endlessly.
            const bool AUTO_REWIND = true;

            player = new Player(url, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS);
    */

	float start = static_cast <float> (clock ());
	float end;
    // "Working horse."
    while (getModuleStateAndWaitForRemainingTimeInTimeslice() == odcore::data::dmcp::ModuleStateMessage::RUNNING)
        {
            bool has_next_frame = false;

            // Use the shared memory image.
            Container c;
            if (player != NULL)    // Read the next container from file.
                {
                    if (player->hasMoreData())
                        {
                            c = player->getNextContainerToBeSent();
                        }
                    else
                        break;
                }
            else     // Get the most recent available container for a SHARED_IMAGE.
                {

                    c = getKeyValueDataStore().get(SharedImage::ID());
                }

            if (c.getDataType() == SharedImage::ID())
                {
                    // Example for processing the received container.
                    has_next_frame = readSharedImage(c);
                }

            // Process the read image.
            if (true == has_next_frame)
            {
                ++m_frame_count;
                processImage();

                end = static_cast <float> (clock ());

			    float deltaT = ( end - start) / (static_cast <float> (CLOCKS_PER_SEC));
				float fps = 1.0f / deltaT;
			    cout << "Delta: " << (int)(deltaT*1000) << "ms, fps: " << fps << endl;
			    start = static_cast <float> (clock ());
            }
        }

    OPENDAVINCI_CORE_DELETE_POINTER(player);

    if (m_debug)
    {
		waitKey(20);
	}



    return odcore::data::dmcp::ModuleExitCodeMessage::OKAY;
}
// All the showResult_* functions assumes that data is put in the sub result structs in LineDetector.

void LaneDetector::showResult(LineDetector &road, Mat &f)
{
    showResult_getRectangles(road.getResult_getRectangles()->rects, f);

    // Fetch pointers to result data
    FinalOutput *res_createTrajectory = road.getResult_createTrajectory();

    // Show final result window
    showResult_createTrajectory(res_createTrajectory, road, f);

    // Create window to display text results
    Mat txtRes(150, 300, CV_8UC3, Scalar(0));

    ostringstream convert;
    int rB = 0; // Pixel where the row starts at
    int rS = 15; // The row interleaving in pixels
    string text;

    // ----extractRoad() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_extractRoad;
    text = convert.str() + " - extractRoad()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);


    // ----extractLines() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_extractLines;
    text = convert.str() + " - extractLines()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

//    // ----getContours() -----
//
//    rB += rS;
//    convert.str("");
//    convert << road.time_taken_contour;
//    text = convert.str() + " - getContours()";
//    cv::putText(txtRes, text, cv::Point(1, rB),
//                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
//
//    // ----getRectangles() -----
//
//    rB += rS;
//    convert.str("");
//    convert << road.time_taken_find_lines;
//    text = convert.str() + " - getRectangles()";
//    cv::putText(txtRes, text, cv::Point(1, rB),
//                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
//
//    // ----classification() -----
//
//    rB += rS;
//    convert.str("");
//    convert << road.time_taken_classification;
//    text = convert.str() + " - classification()";
//    cv::putText(txtRes, text, cv::Point(1, rB),
//                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
//
//    // ----characteristicFiltering() -----
//
//    rB += rS;
//    convert.str("");
//    convert << road.time_taken_characteristicFiltering;
//    text = convert.str() + " - characteristicFiltering()";
//    cv::putText(txtRes, text, cv::Point(0, rB),
//                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
//
    // ----createTrajectory() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_createTrajectory;
    text = convert.str() + " - createTrajectory()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    imshow("Text results", txtRes);
}
void LaneDetector::showResult_finalFilter(IntermediateResult *res, LineDetector &road, Mat &f)
{
	bool printouts = false;
    Mat frame = f.clone();

    if (printouts)
        {
            cout << "__START: Result after finalFilter " << endl;
            cout << "Dashes: " << res->cntDash << endl;
            cout << "Solids: " << res->cntSolid << endl;
            cout << "Intersection: " << res->intersectionOn << endl;
        }
    print_lines(res, frame);

    imshow("Result from finalFilter", frame);
    if (printouts)
        {
            cout << "__END: Result after finalFilter" << endl;
        }
    frame.release();
}

void LaneDetector::showResult_getRectangles(vector<RotatedRect> rects, Mat &f) {
    Mat frame = f.clone();

    RotatedRect rect;

    for (int i = 0; i < rects.size(); i++) {
        rect = rects[i];
        Point2f rect_points[4];
        rect.points(rect_points);

        for (int j = 0; j < 4; j++) {
            line(frame, rect_points[j], rect_points[(j + 1) % 4], Scalar(255), 2);
        }
    }

    imshow("Result from getRectangles", frame);
    frame.release();
}

void LaneDetector::showResult_createTrajectory(FinalOutput *res, LineDetector &road, Mat &f)
{
	bool printouts = false;

    Mat frame = f.clone();

    if (printouts)
        {
            cout << "__START: createTrajectory" << endl;
        }
    if (res->noTrajectory == true){
        if (res->intersection_goalLine == true){
            line(frame, res->rightGoalLines[0].p1, res->rightGoalLines[0].p2, Scalar(153, 106, 0), 2, CV_AA);
            line(frame, res->leftGoalLines[0].p1, res->leftGoalLines[0].p2, Scalar(153, 0, 76), 2, CV_AA);
			line(frame, res->currentLine.p1, res->currentLine.p2, Scalar(255, 0, 255), 2, CV_AA);

            if (m_debug && printouts)
                {
                	cout << "Intersection goalLine:" << endl;
                    cout << "rightGoalLines[" << 0 << "] slope: " << res->rightGoalLines[0].slope << " p1(" << res->rightGoalLines[0].p1.x << "," << res->rightGoalLines[0].p1.y;
                    cout << ") p2(" << res->rightGoalLines[0].p2.x << "," << res->rightGoalLines[0].p2.y << ")" << endl;
                    cout << "leftGoalLines[" << 0 << "] slope: " << res->leftGoalLines[0].slope << " p1(" << res->leftGoalLines[0].p1.x << "," << res->leftGoalLines[0].p1.y;
                    cout << ") p2(" << res->leftGoalLines[0].p2.x << "," << res->leftGoalLines[0].p2.y << ")" << endl;

                }
        }else
            cout << "Nothing in..." << endl;

    }else{
		for (int i = 0; i < res->rightGoalLines.size(); i++){
			if (res->estimatedLeft[i])
				line(frame, res->left[i].p1, res->left[i].p2, Scalar(255, 0, 0), 1, CV_AA);
			else
				line(frame, res->left[i].p1, res->left[i].p2, Scalar(255, 0, 0), 2, CV_AA);

			if (res->estimatedRight[i])
				line(frame, res->right[i].p1, res->right[i].p2, Scalar(0, 0, 255), 1, CV_AA);
			else
				line(frame, res->right[i].p1, res->right[i].p2, Scalar(0, 0, 255), 2, CV_AA);

			if (res->estimatedDash[i])
				line(frame, res->dash[i].p1, res->dash[i].p2, Scalar(0, 255, 0), 1, CV_AA);
			else
				line(frame, res->dash[i].p1, res->dash[i].p2, Scalar(0, 255, 0), 2, CV_AA);

			line(frame, res->rightGoalLines[i].p1, res->rightGoalLines[i].p2, Scalar(153, 106, 0), 2, CV_AA);
			line(frame, res->leftGoalLines[i].p1, res->leftGoalLines[i].p2, Scalar(153, 0, 76), 2, CV_AA);
		}
		line(frame, res->currentLine.p1, res->currentLine.p2, Scalar(255, 0, 255), 2, CV_AA);

		for (int i = 0; i < res->cutPoints.size(); i++){
			Point p;
			p.y = res->cutPoints[i];
			p.x = 0;
			Point q = p;
			q.x = 800;
			line(frame, p, q, Scalar(255, 255, 255), 1, CV_AA);
		}

		if (printouts){
			for (int i = 0; i < res->cutPoints.size(); i++){
				cout << "cutPoint: " << res->cutPoints[i] << endl;
			}
			for (int i = 0; i < res->rightGoalLines.size(); i++){
				cout << "left[" << i << "] slope: " << res->left[i].slope << " p1(" << res->left[i].p1.x << "," << res->left[i].p1.y;
				cout << ") p2(" << res->left[i].p2.x << "," << res->left[i].p2.y << ")" << endl;

				cout << "Dashed[" << i << "] slope: " << res->dash[i].slope << " p1(" << res->dash[i].p1.x << "," << res->dash[i].p1.y;
				cout << ") p2(" << res->dash[i].p2.x << "," << res->dash[i].p2.y << ")" << endl;

				cout << "right[" << i << "] slope: " << res->right[i].slope << " p1(" << res->right[i].p1.x << "," << res->right[i].p1.y;
				cout << ") p2(" << res->right[i].p2.x << "," << res->right[i].p2.y << ")" << endl;


				cout << "leftGoalLines[" << i << "] slope: " << res->leftGoalLines[i].slope << " p1(" << res->leftGoalLines[i].p1.x << "," << res->leftGoalLines[i].p1.y;
				cout << ") p2(" << res->leftGoalLines[i].p2.x << "," << res->leftGoalLines[i].p2.y << ")" << endl;

				cout << "rightGoalLines[" << i << "] slope: " << res->rightGoalLines[i].slope << " p1(" << res->rightGoalLines[i].p1.x << "," << res->rightGoalLines[i].p1.y;
				cout << ") p2(" << res->rightGoalLines[i].p2.x << "," << res->rightGoalLines[i].p2.y << ")" << endl;

				cout << "---" << endl;
			}
		}
	}

    imshow("Result from createTrajectory", frame);
    if (printouts)
        {
            cout << "__END: createTrajectory" << endl;
        }
    frame.release();
}
void LaneDetector::print_lines(IntermediateResult *res, Mat &f)
{
	bool printouts = false;
    for (int i = 0; i < res->cntDash; i++)
        {
            line(f, res->dashLines[i].p1, res->dashLines[i].p2, 45, 2);
            if (printouts)
                {
                    cout << "Dash line angle: " << res->dashLines[i].slope << endl;
                }
        }
    for (int i = 0; i < res->cntSolid; i++)
        {
            line(f, res->solidLines[i].p1, res->solidLines[i].p2, 0, 2);
            if (printouts)
                {
                    cout << "Solid line angle: " << res->solidLines[i].slope << endl;
                }
        }
}
} // msv


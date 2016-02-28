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

#include "opendavinci/odcore/opendavinci.h"
#include "opendavinci/odcore/base/KeyValueConfiguration.h"
#include "opendavinci/odcore/data/Container.h"
//#include "opendavinci/odcore/data/image/SharedImage.h" included by GeneratedHeaders_AutomotiveData.h
#include "opendavinci/GeneratedHeaders_OpenDaVINCI.h"

#include "opendavinci/odcore/io/conference/ContainerConference.h"
#include "opendavinci/odcore/wrapper/SharedMemoryFactory.h"

#include "opendavinci/odtools/player/Player.h"

// Data structures from msv-data library:
//#include "SteeringData.h" included by GeneratedHeaders_AutomotiveData.h
//#include "SteeringData.h"

#include "LaneDetector_inspection.h"

namespace msv
{

using namespace std;
using namespace odcore::base;
using namespace odcore::data;
using namespace odcore::data::image;
using namespace odtools::player;
using namespace cv;

bool debug, print_results = true;
Config cfg;
// Global variables for camera functions
int  avg_time = 0, num_msmnt;
double width = 752, height = 480;
Mat img;
int key;
long time_taken_get_line;
LaneDetector_inspection::LaneDetector_inspection(const int32_t &argc, char **argv) :
    TimeTriggeredConferenceClientModule(argc, argv, "lanedetector"),
    m_hasAttachedToSharedImageMemory(false),
    m_sharedImageMemory(),
    m_cameraId(-1),
    m_debug(false),
    m_config(),
    m_frame(),
    m_frame_count(0)
{
    m_config.th1 = 70;//83;
    m_config.th2 = 230;
    m_config.hlTh = THRESH_BINARY;
    m_config.XTimesYMin = 2;
    m_config.XTimesYMax = 20;
    m_config.maxY = 235;//195;
    m_config.maxArea = 4;
}

LaneDetector_inspection::~LaneDetector_inspection() {}

void LaneDetector_inspection::setUp()
{
    // This method will be call automatically _before_ running body().
    if (m_debug)
        {
            // Create an OpenCV-window.
            cvNamedWindow("WindowShowImage", CV_WINDOW_AUTOSIZE);
            cvMoveWindow("WindowShowImage", 300, 100);
        }
}

void LaneDetector_inspection::tearDown()
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

bool LaneDetector_inspection::readSharedImage(Container &c)
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
                                m_frame = rawImg;
                                //memcpy(m_image->imageData,
                                //   (char *)m_sharedImageMemory->getSharedMemory(),
                                //  width * height * numberOfChannels);
                                cvReleaseImage(&image);
                            }
                    }

                    // Release the memory region so that the image produce (i.e. the camera for example) can provide the next raw image data.
                    m_sharedImageMemory->unlock();
                    cout << "Memory unlock" << endl;
                    // Mirror the image.
                    //cvFlip(m_image, 0, -1);

                    retVal = true;
                }
        }
    return retVal;
}

// You should start your work in this method.
void LaneDetector_inspection::processImage()
{

    TimeStamp currentTime_strt1;

    debug = m_debug;
    cout << "Debug: " << debug << endl;
    cfg = m_config;

    Mat neededPart = m_frame(cv::Rect(1, 2 * height / 16 - 1, width - 1, 10 * height / 16 - 1));

    LineDetector road(neededPart, cfg, debug, 1);

    if (debug)
        showResult(road, neededPart);

    // Start fix. This code deactivates the old estimateLines and calculatesGoalLine()
    //msv::Lines lines = road.getLines();
    msv::Lines lines = *(new Lines());
    // End fix.

    msv::LaneDetectorDataToDriver dataToDriver = *(road.getDriverData());

        
    // if (lines != NULL)
    //  cout << "We have lines for frame " <<m_frame_count << endl;
    LaneDetectionData data;
    data.setLaneDetectionData(lines, dataToDriver);
    data.setFrameCount(m_frame_count);

    //some more mess
    TimeStamp currentTime_strt7;
    double timeStep_total = (currentTime_strt7.toMicroseconds()
                             - currentTime_strt1.toMicroseconds()) / 1000.0;
    if (debug)
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
    if (debug)
        {
            cout << dec;
            cout << "avg_time: " << avg_time << "ms" << endl;
        }


    //          std::ofstream log_file(
    //                                  "/opt/msv/bin/2013/DIT-168/project-template/log_file.txt", std::ios_base::out | std::ios_base::app );
    //                           long time_result = pow(time_taken_mul, 1.0/frame_count);
    //                              log_file << "Time consumed " << time_result << endl;

    //end of the mess

    //Inspection part. move to function later
    string classification;
    int skip_to_frame = 1;  // Use this variable to fast-forward to a specific frame
    if (m_frame_count < skip_to_frame)
        {
            classification = "N/A";
        }
    else
        {
            bool status = true;
            cvWaitKey(50);
            char key = cvWaitKey(0);

            cout << key << endl;
            while (status)
                {
                    switch (key)
                        {
                            cout << key << endl;
                        case 49:
                            classification = "TP";
                            status = false;
                            break;
                        case 50:
                            classification = "TN";
                            status = false;
                            break;
                        case 51:
                            classification = "FP";
                            status = false;
                            break;
                        case 52:
                            classification = "FN";
                            status = false;
                            break;
                        default:
                            status = true;
                            cvWaitKey(50);
                            key = cvWaitKey(0);
                            break;
                        }
                }
        }
    data.setClassification(classification);
    cout << "classification : " + classification << endl;
    // end of inspection part --Need to make in nicer way --

    //seding all data to csv
    Container con(data);

    // Send the data:
    //cout << "Send..." << endl;
    getConference().send(con);


    m_frame.release();
    waitKey(20);

}

// This method will do the main data processing job.
// Therefore, it tries to open the real camera first. If that fails, the virtual camera images from camgen are used.
odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode LaneDetector_inspection::body()
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
    //m_inspection = kv.getValue<u_int32_t>("lanedetector.inspection")==1;


    // Lane-detector can also directly read the data from file. This might be interesting to inspect the algorithm step-wisely
    odcore::io::URL url(kv.getValue<string>("lanedetector.inspect_rec"));


    // Size of the memory buffer.
    const uint32_t MEMORY_SEGMENT_SIZE = kv.getValue<uint32_t>("global.buffer.memorySegmentSize");

    // Number of memory segments.
    const uint32_t NUMBER_OF_SEGMENTS = kv.getValue<uint32_t>("global.buffer.numberOfMemorySegments");

    // The output windows to display
    showRes_getContours = kv.getValue<bool>("lanedetector.showRes_getContours");
    showRes_getRectangles = kv.getValue<bool>("lanedetector.showRes_getRectangles");
    showRes_classification = kv.getValue<bool>("lanedetector.showRes_classification");
    showRes_filterAndMerge = kv.getValue<bool>("lanedetector.showRes_filterAndMerge");
    showRes_finalFilter = kv.getValue<bool>("lanedetector.showRes_finalFilter");
    showRes_finalResult = kv.getValue<bool>("lanedetector.showRes_finalResult");
    showRes_createTrajectory = kv.getValue<bool>("lanedetector.showRes_createTrajectory");
    what_to_inspect = kv.getValue<uint32_t>("lanedetector.what_to_inspect");

    // If AUTO_REWIND is true, the file will be played endlessly.
    const bool AUTO_REWIND = false;

    // Run player in synchronous mode with data caching in background.
    const bool THREADING = false;

    player = new Player(url, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS, THREADING);


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
                }

        }

    OPENDAVINCI_CORE_DELETE_POINTER(player);

    waitKey(20);
    return odcore::data::dmcp::ModuleExitCodeMessage::OKAY;
}

// All the showResult_* functions assumes that data is put in the sub result structs in LineDetector.

void LaneDetector_inspection::showResult(LineDetector &road, Mat &f)
{
    // Fetch pointers to result data
    IntermediateResult_getContours *res_getContours = road.getResult_getContours();
    IntermediateResult_getRectangles *res_getRectangles = road.getResult_getRectangles();
    IntermediateResult *res_classification = road.getResult_classification();
    IntermediateResult *res_filterAndMerge = road.getResult_filterAndMerge();
    IntermediateResult *res_finalFilter = road.getResult_finalFilter();
    LinesToUse *res_finalResult = road.getResult_calculateGoalLine();
    FinalOutput *res_createTrajectory = road.getResult_createTrajectory();

    // Show result windows
    if (showRes_getContours)
        showResult_getContours(res_getContours, road, f);

    if (showRes_getRectangles)
        showResult_getRectangles(res_getRectangles, road, f);

    if (showRes_classification)
        showResult_classification(res_classification, road, f);

    if (showRes_filterAndMerge)
        showResult_filterAndMerge(res_filterAndMerge, road, f);

    if (showRes_finalFilter)
        showResult_finalFilter(res_finalFilter, road, f);

    if (showRes_finalResult)
        showResult_finalResult(res_finalResult, road, f);

    if (showRes_createTrajectory)
        showResult_createTrajectory(res_createTrajectory, road, f);

    // Create window to display text results
    cv::Mat txtRes = cv::Mat::zeros(500, 330, CV_8UC3);

    ostringstream convert;
    int rB = 0; // Pixel where the row starts at
    int rS = 15; // The row interleaving in pixels
    string text;

    rB += rS;
    convert.str("");
    convert << m_frame_count;
    text = "Frame: " + convert.str();
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    // ----extractRoad() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_extractRoad;
    text = convert.str() + " - extractRoad()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    // ----getContours() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_contour;
    text = convert.str() + " - getContours()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    rB += rS;
    convert.str("");
    convert << res_getContours->contours.size();
    text = "contours found: " + convert.str();
    cv::putText(txtRes, text, cv::Point(20, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    // ----getRectangles() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_find_lines;
    text = convert.str() + " - getRectangles()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    rB += rS;
    convert.str("");
    convert << res_getRectangles->rects.size();
    text = "rectangles found: " + convert.str();
    cv::putText(txtRes, text, cv::Point(20, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    // ----classification() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_classification;
    text = convert.str() + " - classification()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    // ----characteristicFiltering() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_characteristicFiltering;
    text = convert.str() + " - characteristicFiltering()";
    cv::putText(txtRes, text, cv::Point(0, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    rB += rS;
    cv::putText(txtRes, "Dashed curve: ", cv::Point(20, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
    if (res_finalResult->dashedCurveFound)
        {
            putText(txtRes, "Found #:", cvPoint(140, rB),
                    FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
            convert.str("");
            convert << res_finalResult->dashedCurve.size();
            cv::putText(txtRes, convert.str(), cv::Point(220, rB),
                        FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

        }
    else
        {
            if (res_finalResult->cntDash < 2)
                {
                    putText(txtRes, "More dashes needed", cvPoint(140, rB),
                            FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

                }
            else
                {
                    putText(txtRes, "FAILURE TO FIND CURVE", cvPoint(140, rB),
                            FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
                }
        }

    rB += rS;
    cv::putText(txtRes, "Found left:   dashed:   right:", cv::Point(20, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
    convert.str("");
    convert << res_finalResult->foundL;
    cv::putText(txtRes, convert.str(), cv::Point(120, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
    convert.str("");
    convert << res_finalResult->foundD;
    cv::putText(txtRes, convert.str(), cv::Point(200, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);
    convert.str("");
    convert << res_finalResult->foundR;
    cv::putText(txtRes, convert.str(), cv::Point(270, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    // ----createTrajectory() -----

    rB += rS;
    convert.str("");
    convert << road.time_taken_createTrajectory;
    text = convert.str() + " - createTrajectory()";
    cv::putText(txtRes, text, cv::Point(1, rB),
                FONT_HERSHEY_COMPLEX_SMALL, 0.65, cv::Scalar(0, 255, 0), 1, CV_AA);

    imshow("Text results", txtRes);
}

void LaneDetector_inspection::showResult_getContours(IntermediateResult_getContours *res, LineDetector &road, Mat &f)
{
    //Mat frame = f.clone();
    Mat frame(f.rows, f.cols, CV_8UC1, Scalar(0, 0, 0));

    if (m_debug && print_results)
        {
            cout << "__START: All found contours" << endl;
            cout << "NOT IMPLEMENTED" << endl;
            cout << "__END: All found contours" << endl;
        }

    // TODO add the contours to the frame
    drawContours(frame, res->contours, -1, Scalar(255, 0, 0), -1, 8);
    imshow("All contours", frame);
    if (what_to_inspect == 1)
        addInspectionInfo(road, frame);

    frame.release();
}

void LaneDetector_inspection::showResult_getRectangles(IntermediateResult_getRectangles *res, LineDetector &road, Mat &f)
{
    //Mat frame = f.clone();
    Mat frame(f.rows, f.cols, CV_8UC1, Scalar(0, 0, 0));

    if (m_debug && print_results)
        {
            cout << "__START: All found rectangles" << endl;
            cout << res->rects.size() << " rectangles." << endl;
            cout << "__END: All found rectangles" << endl;
        }

    // Print rectangles to the frame
    for (int j = 0; j < res->rects.size(); j++)
        {
            Point2f vertices[4];
            res->rects[j].points(vertices);
            for (int i = 0; i < 4; i++)
                line(frame, vertices[i], vertices[(i + 1) % 4], Scalar(255, 255, 255));
        }
    if (what_to_inspect == 2)
        addInspectionInfo(road, frame);

    imshow("All rectangles", frame);
    frame.release();
}

void LaneDetector_inspection::showResult_classification(IntermediateResult *res, LineDetector &road, Mat &f)
{
    Mat frame = f.clone();

    if (m_debug && print_results)
        {
            cout << "__START: Result after classification " << endl;
            cout << "Dashes: " << res->cntDash << endl;
            cout << "Solids: " << res->cntSolid << endl;
            cout << "Intersection: " << res->intersectionOn << endl;
        }
    print_lines(res, frame);

    if (what_to_inspect == 3)
        addInspectionInfo(road, frame);

    imshow("Result from classification", frame);
    if (m_debug && print_results)
        {
            cout << "__END: Result after classification" << endl;
        }
    frame.release();
}

void LaneDetector_inspection::showResult_filterAndMerge(IntermediateResult *res, LineDetector &road, Mat &f)
{
    Mat frame = f.clone();

    if (m_debug && print_results)
        {
            cout << "__START: Result after filterAndMerge " << endl;
            cout << "Dashes: " << res->cntDash << endl;
            cout << "Solids: " << res->cntSolid << endl;
            cout << "Intersection: " << res->intersectionOn << endl;
        }
    print_lines(res, frame);

    if (what_to_inspect == 4)
        addInspectionInfo(road, frame);

    imshow("Result from filterAndMerge", frame);
    if (m_debug && print_results)
        {
            cout << "__END: Result after filterAndMerge" << endl;
        }
    frame.release();
}

void LaneDetector_inspection::showResult_finalFilter(IntermediateResult *res, LineDetector &road, Mat &f)
{
    Mat frame = f.clone();

    if (m_debug && print_results)
        {
            cout << "__START: Result after finalFilter " << endl;
            cout << "Dashes: " << res->cntDash << endl;
            cout << "Solids: " << res->cntSolid << endl;
            cout << "Intersection: " << res->intersectionOn << endl;
        }
    print_lines(res, frame);

    if (what_to_inspect == 5)
        addInspectionInfo(road, frame);

    imshow("Result from finalFilter", frame);
    if (m_debug && print_results)
        {
            cout << "__END: Result after finalFilter" << endl;
        }
    frame.release();
}

void LaneDetector_inspection::showResult_finalResult(LinesToUse *res, LineDetector &road, Mat &f)
{
    Mat frame = f.clone();

    if (m_debug && print_results)
        {
            cout << "__START: Final result" << endl;
        }
    if (res->lines->goalLine.p1.x == 0 && res->lines->goalLine.p1.y == 0
            && res->lines->goalLine.p2.x == 0 && res->lines->goalLine.p2.y == 0
            && res->lines->currentLine.p2.x == 0 && res->lines->currentLine.p2.y == 0)
        {
            cout << "Nothing in..." << endl;
        }
    else
        {
            drawLines(res->lines, &frame, 0);
        }

    if (m_debug && print_results)
        {
            cout << "Found lines, left: " << res->foundL << ". dashed: " << res->foundD << ". right: " << res->foundR << endl;
            cout << "Left line. p1(" << res->leftLine.p1.x << "," << res->leftLine.p1.y << ") p2(" << res->leftLine.p2.x << "," << res->leftLine.p2.y << ")" << endl;
            cout << "Dashed line. p1(" << res->dashLine.p1.x << "," << res->dashLine.p1.y << ") p2(" << res->dashLine.p2.x << "," << res->dashLine.p2.y << ")" << endl;
            cout << "Right line. p1(" << res->rightLine.p1.x << "," << res->rightLine.p1.y << ") p2(" << res->rightLine.p2.x << "," << res->rightLine.p2.y << ")" << endl;

            cout << "VP [x, y] : [" << res->lines->goalLine.p1.x << ", "
                 << res->lines->goalLine.p1.y << "]" << endl;
            cout << "Goal [x, y] : [" << res->lines->goalLine.p2.x << ", "
                 << res->lines->goalLine.p2.y << "]" << endl;
            cout << "Position [x, y] : [" << res->lines->currentLine.p2.x << ", "
                 << res->lines->currentLine.p2.y << "]" << endl;
        }
    if (what_to_inspect == 6)
        addInspectionInfo(road, frame);

    imshow("Final result", frame);
    if (m_debug && print_results)
        {
            cout << "__END: Final result" << endl;
        }
    frame.release();
}
Point LaneDetector_inspection::pad(int* top, int* left, cv::Point& p)
{   
    Point padded;
    padded.x = p.x + *left;
    padded.y = p.y + *top;
    return padded;
}
void LaneDetector_inspection::showResult_createTrajectory(FinalOutput *res, LineDetector &road, Mat &f)
{
    int top = 100;
    int bottom = 100;
    int left = 400;
    int right = 400;
    Mat frame, src;
    cvtColor(f, src, CV_GRAY2BGR);
    namedWindow("Result from createTrajectory", CV_WINDOW_NORMAL);
    copyMakeBorder( src, frame, top, bottom, left, right, BORDER_CONSTANT, Scalar(30, 30, 30));
    cvResizeWindow("Result from createTrajectory", 1300, 400);

    // TODO: display switchPoints

    if (m_debug && print_results)
        {
            cout << "__START: createTrajectory" << endl;
        }
    if (res->noTrajectory == true){
        if (res->intersection_goalLine == true){
            line(frame, pad(&top,&left,res->currentLine.p1), pad(&top,&left,res->currentLine.p2), Scalar(255, 128, 0), 2, CV_AA);
            line(frame, pad(&top,&left,res->rightGoalLines[0].p1), pad(&top,&left,res->rightGoalLines[0].p2), Scalar(153, 0, 76), 2, CV_AA);
            line(frame, pad(&top,&left,res->leftGoalLines[0].p1), pad(&top,&left,res->leftGoalLines[0].p2), Scalar(0, 128, 255), 2, CV_AA);
                
            if (m_debug && print_results)
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
                    line(frame, pad(&top,&left,res->left[i].p1), pad(&top,&left,res->left[i].p2), Scalar(255, 0, 0), 1, CV_AA);
                else
                    line(frame, pad(&top,&left,res->left[i].p1), pad(&top,&left,res->left[i].p2), Scalar(255, 0, 0), 2, CV_AA);

                if (res->estimatedRight[i])
                    line(frame, pad(&top,&left,res->right[i].p1), pad(&top,&left,res->right[i].p2), Scalar(0, 0, 255), 1, CV_AA);
                else
                    line(frame, pad(&top,&left,res->right[i].p1), pad(&top,&left,res->right[i].p2), Scalar(0, 0, 255), 2, CV_AA);

                if (res->estimatedDash[i])
                    line(frame, pad(&top,&left,res->dash[i].p1), pad(&top,&left,res->dash[i].p2), Scalar(0, 255, 0), 1, CV_AA);
                else
                    line(frame, pad(&top,&left,res->dash[i].p1), pad(&top,&left,res->dash[i].p2), Scalar(0, 255, 0), 2, CV_AA);

                line(frame, pad(&top,&left,res->rightGoalLines[i].p1), pad(&top,&left,res->rightGoalLines[i].p2), Scalar(153, 0, 76), 2, CV_AA);
                line(frame, pad(&top,&left,res->leftGoalLines[i].p1), pad(&top,&left,res->leftGoalLines[i].p2), Scalar(0, 128, 255), 2, CV_AA);
            }
            line(frame, pad(&top,&left,res->currentLine.p1), pad(&top,&left,res->currentLine.p2), Scalar(255, 128, 0), 2, CV_AA);

            for (int i = 0; i < res->cutPoints.size(); i++){
                Point p;
                p.y = res->cutPoints[i];
                p.x = 0;
                Point q = p;
                q.x = 752;
                line(frame, pad(&top,&left,p), pad(&top,&left,q), Scalar(255, 255, 255), 1, CV_AA);
            }

        if (m_debug && print_results)
            {
            for (int i = 0; i < res->cutPoints.size(); i++){
                cout << "cutPoint: " << res->cutPoints[i] << endl;
            }
            for (int i = 0; i < res->rightGoalLines.size(); i++)
                {
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
    if (what_to_inspect == 7)
        addInspectionInfo(road, frame);

    imshow("Result from createTrajectory", frame);
    if (m_debug && print_results)
        {
            cout << "__END: createTrajectory" << endl;
        }
    frame.release();
}

void LaneDetector_inspection::addInspectionInfo(LineDetector &road, Mat &frame)
{

    //converting current frame count into string
    string current_frame;
    ostringstream convert;
    convert << m_frame_count;
    current_frame = convert.str();
    //end of converting --Need find better way for this ---

    //Putting text on the image for the 4 cases and current frame number
    putText(frame, "TP - 1", cvPoint(30, 20),
            FONT_HERSHEY_COMPLEX_SMALL, 0.8, cvScalar(255, 128, 0), 1, CV_AA);
    putText(frame, "TN - 2", cvPoint(30, 40),
            FONT_HERSHEY_COMPLEX_SMALL, 0.8, cvScalar(255, 128, 0), 1, CV_AA);
    putText(frame, "FP - 3", cvPoint(30, 60),
            FONT_HERSHEY_COMPLEX_SMALL, 0.8, cvScalar(255, 128, 0), 1, CV_AA);
    putText(frame, "FN - 4", cvPoint(30, 80),
            FONT_HERSHEY_COMPLEX_SMALL, 0.8, cvScalar(255, 128, 0), 1, CV_AA);
    putText(frame, "Frame", cvPoint(30, 100),
            FONT_HERSHEY_COMPLEX_SMALL, 0.8, cvScalar(255, 128, 0), 1, CV_AA);
    putText(frame, current_frame, cvPoint(100, 100),
            FONT_HERSHEY_COMPLEX_SMALL, 0.8, cvScalar(255, 128, 0), 1, CV_AA);
}

void LaneDetector_inspection::drawLines(msv::Lines *lines, Mat *dst, int offset)
{
    Line dashed = lines->dashedLine;
    Line solidRight = lines->rightLine;
    Line solidLeft = lines->leftLine;

    line( *dst, Point(dashed[0], dashed[1] + offset), Point(dashed[2], dashed[3] + offset), Scalar(0, 255, 0), 3, CV_AA);
    line( *dst, Point(solidRight[0], solidRight[1] + offset), Point(solidRight[2], solidRight[3] + offset), Scalar(255, 0, 0), 3, CV_AA);
    line( *dst, Point(solidLeft[0], solidLeft[1] + offset), Point(solidLeft[2], solidLeft[3] + offset), Scalar(255, 255, 255), 3, CV_AA);
    line( *dst, lines->goalLine.p1, lines->goalLine.p2, Scalar(200, 0, 0), 3, CV_AA);
    line( *dst, lines->currentLine.p1, lines->currentLine.p2, Scalar(200, 0, 0), 3, CV_AA);
}

void LaneDetector_inspection::print_lines(IntermediateResult *res, Mat &f)
{
    for (int i = 0; i < res->cntDash; i++)
        {
            line(f, res->dashLines[i].p1, res->dashLines[i].p2, 45, 2);
            if (m_debug && print_results)
                {
                    cout << "Dash line angle: " << res->dashLines[i].slope << endl;
                }
        }
    for (int i = 0; i < res->cntSolid; i++)
        {
            line(f, res->solidLines[i].p1, res->solidLines[i].p2, 0, 2);
            if (m_debug && print_results)
                {
                    cout << "Solid line angle: " << res->solidLines[i].slope << endl;
                }
        }
}
} // msv


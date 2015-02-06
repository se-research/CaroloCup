#include "LineDetector.h"
#include <stdio.h>
#include <math.h>
#include "Transforms.h"

#include "opencv2/core/core.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>
#include <fstream>

#include "nnRoadSizeCalc.h"
#include "nnRoadAngleCalc.h"

#include "core/data/TimeStamp.h"

namespace msv
{

using namespace core::data;
using namespace std;
using namespace cv;

int cntDash = 0;
int cntSolid = 0;
vector<CustomLine> dashLines;
vector<CustomLine> solidLines;
vector<CustomLine> global_dashedCurve;
int h, w, offset;
int roadSize = ROAD_SIZE;
int roadAngle = 91;
bool foundStopStartLine = false;
int currentDashGoalX = 0;
int currentRightGoalX = 0;
int currentLeftGoalX = 0;
int calcRoadSize, calcRoadAngle;
float minXI, minYI, YI;
long startTime;

bool intersectionOn = false;
bool foundIntersection = false;
RoadState roadState=NORMAL;
int intersectionRect = -1;


LineDetector::LineDetector(const Mat &f, const Config &cfg, const bool debug,
                           const int id) :
    m_frame(), m_frameCanny(), m_lines(NULL), m_debug(debug), m_lastSolidRightTop(), detectedLines(), m_config(
        cfg), confidenceLevel(0)
{
    cout << "___start LineDetector" << endl;
    m_frame = f.clone();
    Mat outputImg = f.clone();
    //if (m_debug)
    //imshow("m_frame",m_frame);
    w = m_frame.size().width;
    h = m_frame.size().height;
    offset = 2 * h / 16 - 1;
    /// Detect edges using Threshold
    threshold(m_frame, m_frame, cfg.th1, 255, CV_THRESH_BINARY);
    cvtColor(m_frame, m_frame_color, CV_GRAY2BGR);

    // Run lineDetector and provide goalLines for the driver
    findLines(outputImg);
    //cout << "Id:" << id << endl;
    cout << "___end LineDetector" << endl;
}

LineDetector::~LineDetector()
{
    if (NULL != m_lines)
        {
            delete m_lines;
        }
}


RoadState LineDetector::getRoadState(){
    	return roadState;//not really a good idea returning a global value
    }
// This function exists due to old conventions
Lines LineDetector::getLines()
{
    return *(ltu.lines);
}

// The "body"
void LineDetector::findLines(cv::Mat &outputImg)
{

    if (m_debug)
        {
            TimeStamp currentTime;
            startTime = currentTime.toMicroseconds();
        }
    //Find contours
    getContours(outputImg);

    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_contour = endTime.toMicroseconds() - startTime;
            result_getContours.contours = contours_poly;
            TimeStamp currentTime;
            startTime = currentTime.toMicroseconds();
        }
    //Get all marked lines
    getRectangles();
    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_find_lines = endTime.toMicroseconds() - startTime;
            result_getRectangles.rects = rects;
            TimeStamp currentTime;
            startTime = currentTime.toMicroseconds();
        }

    //Classify dash lines and solid lines
    classification();
    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_classification = endTime.toMicroseconds() - startTime;
            result_classification.dashLines = dashLines;
            result_classification.solidLines = solidLines;
            result_classification.cntDash = cntDash;
            result_classification.cntSolid = cntSolid;
            result_classification.foundStopStartLine = foundStopStartLine;
            result_classification.intersectionOn = intersectionOn;
            result_classification.foundIntersection = foundIntersection;
            TimeStamp currentTime;
            startTime = currentTime.toMicroseconds();
        }

    //Filter dashes outside the solid lines and merge solid lines
    filterAndMerge();
    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_filter_merge = endTime.toMicroseconds() - startTime;
            result_filterAndMerge.dashLines = dashLines;
            result_filterAndMerge.solidLines = solidLines;
            result_filterAndMerge.cntDash = cntDash;
            result_filterAndMerge.cntSolid = cntSolid;
            result_filterAndMerge.foundStopStartLine = foundStopStartLine;
            result_filterAndMerge.intersectionOn = intersectionOn;
            result_filterAndMerge.foundIntersection = foundIntersection;
            TimeStamp currentTime;
            startTime = currentTime.toMicroseconds();
        }

    //Filter lines with very small angles, filter dash positioned too high on the image or too left or too right
    finalFilter();
    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_final_filter = endTime.toMicroseconds() - startTime;
            result_finalFilter.dashLines = dashLines;
            result_finalFilter.solidLines = solidLines;
            result_finalFilter.cntDash = cntDash;
            result_finalFilter.cntSolid = cntSolid;
            result_finalFilter.foundStopStartLine = foundStopStartLine;
            result_finalFilter.intersectionOn = intersectionOn;
            result_finalFilter.foundIntersection = foundIntersection;
        }



    characteristicFiltering(&ltu);
    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_characteristicFiltering = endTime.toMicroseconds() - startTime;
            // Debug data is not provided for this function. It is sufficient to look
            // at the debug data from createTrajectory
        }

    // The two commented functions are the old way of creating data to driver
    //estimateLines(&ltu);
    //calculateGoalLine(&ltu);

    createTrajectory(&ltu);
    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_createTrajectory = endTime.toMicroseconds() - startTime;
            // Debug data is set inside the function
        }
    // -- end testing


    // -- testing of new_estimateLines & new_calculateGoalLine:

    // cv::Mat out = m_frame.clone();
    // bool go = false;
    // EstimationData ed;
    // GoalLineData gld;

    // ed.yPosition = h;
    // ed.left = getNoneCustomLine();
    // ed.dash = getNoneCustomLine();
    // ed.right = getNoneCustomLine();
    // if (ltu.foundL)
    //     {
    //         //ed.left = ltu.leftLine;
    //     }
    // if (ltu.foundD)
    //     {
    //         ed.dash = ltu.dashLine;
    //     }
    // if (ltu.foundR)
    //     {
    //         //ed.right = ltu.rightLine;
    //     }

    // provideGoalLine(&ed, &gld);
    // if(ed.left.p2.x == 0 && ed.left.p2.y == 0){
    //     ed.left.p2.x = getIntersectionWithTop(ed.left);
    // }
    // if(ed.dash.p2.x == 0 && ed.dash.p2.y == 0){
    //     ed.dash.p2.x = getIntersectionWithTop(ed.dash);
    // }
    // if(ed.right.p2.x == 0 && ed.right.p2.y == 0){
    //     ed.right.p2.x = getIntersectionWithTop(ed.right);
    // }
    // cout << "left slope: " << ed.left.slope << " x: " << ed.left.p2.x << " y: " << ed.left.p2.y << endl;
    // cout << "dash slope: " << ed.dash.slope << " x: " << ed.dash.p1.x << " y: " << ed.dash.p1.y << endl;
    // cout << "dash slope: " << ed.dash.slope << " x: " << ed.dash.p2.x << " y: " << ed.dash.p2.y << endl;
    // cout << "right slope: " << ed.right.slope << " x: " << ed.right.p2.x << " y: " << ed.right.p2.y << endl;
    // line(out, ed.left.p1, ed.left.p2, Scalar(255, 0, 0));
    // line(out, ed.dash.p1, ed.dash.p2, Scalar(255, 0, 0));
    // line(out, ed.right.p1, ed.right.p2, Scalar(255, 0, 0));

    // cout << "rightGoalLine slope: " << gld.rightGoalLine.slope << " x: " << gld.rightGoalLine.p1.x << " y: " << gld.rightGoalLine.p1.y << endl;
    // cout << "rightGoalLine slope: " << gld.rightGoalLine.slope << " x: " << gld.rightGoalLine.p2.x << " y: " << gld.rightGoalLine.p2.y << endl;
    // line(out, gld.rightGoalLine.p1, gld.rightGoalLine.p2, Scalar(150, 0, 0));

    // cout << "leftGoalLine slope: " << gld.leftGoalLine.slope << " x: " << gld.leftGoalLine.p1.x << " y: " << gld.leftGoalLine.p1.y << endl;
    // cout << "leftGoalLine slope: " << gld.leftGoalLine.slope << " x: " << gld.leftGoalLine.p2.x << " y: " << gld.leftGoalLine.p2.y << endl;
    // line(out, gld.leftGoalLine.p1, gld.leftGoalLine.p2, Scalar(150, 0, 0));

    // imshow("Lines gotten from estimation", out);

    // -- end testing

    // -- testing of intersectionPoint:

    // cv::Mat out = m_frame.clone();

    // cout << "DashedCurve " << endl;
    // for (int i = 1; i < ltu.dashedCurve.size() + 1; i++)
    //     {
    //         cout << "line " << i - 1 << ": (" <<  ltu.dashedCurve[i - 1].p1 << "," << ltu.dashedCurve[i - 1].p2 << ")" << endl;
    //         line(out, ltu.dashedCurve[i - 1].p1, ltu.dashedCurve[i - 1].p2, Scalar(255, 0, 0));
    //     }
    // cout << "DashedCurve end." << endl;
    // imshow("Dashed curve", out);

    // std::vector<CustomLine> ll = ltu.dashedCurve;
    // std::vector<Point> trajectoryPoints = trajectorySwitchingPoints(ltu.dashedCurve);
    // std::vector<CustomLine> res = findCurve(ltu.dashedCurve);

    // cv::Mat out2 = m_frame.clone();
    // cout << "--------------------" << endl << " trajectory points: ";
    // for (int i = 1; i < trajectoryPoints.size(); i++)
    //     {
    //         cout << "(" << trajectoryPoints[i - 1] << "," << trajectoryPoints[i] << ") ";
    //         polylines(out2, trajectoryPoints, false, Scalar(255, 0, 0), 1, CV_AA);
    //     }
    // cout << endl;
    // imshow("Dashed curve trajectory", out2);

    // -- end testing

}

// The "body" functions follow in call order

void LineDetector::getContours(cv::Mat &outputImg)
{
    vector<vector<Point> > contours;
    vector<Vec4i> hierarchy;
    cntDash = 0;
    cntSolid = 0;
    /// Find contours
    findContours(m_frame, contours, hierarchy, CV_RETR_TREE,
                 CV_CHAIN_APPROX_SIMPLE, Point(0, 0));

    /// Make polygon contours
    contours_poly.resize(contours.size());

    dashLines = vector<CustomLine>(contours.size());
    solidLines = vector<CustomLine>(contours.size());
    for (unsigned int i = 0; i < contours.size(); i++)
        {
            approxPolyDP(Mat(contours[i]), contours_poly[i], 3, true);
        }

    return;
}

void LineDetector::getRectangles()
{
    bool picture = false;
    RotatedRect rect;
    cv::Mat out;
    if (picture)
        out = m_frame.clone();

    for (unsigned int i = 0; i < contours_poly.size(); i++)
        {
            rect = minAreaRect(contours_poly[i]);
            Point2f rect_points[4];
            rect.points(rect_points);
            //rects.push_back(rect);
            //cout << "Angle: " << rect.angle << endl;
            int sizeX = 0, sizeY = 0, sizeR = 0;
            Point shortSideMiddle;
            Point longSideMiddle;
            // Find rect sizes
            for (int j = 0; j < 4; j++)
                {
                    //cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y << "]" << endl;
                    sizeR = cv::sqrt(
                                cv::pow((rect_points[j].x - rect_points[(j + 1) % 4].x), 2)
                                + cv::pow(
                                    (rect_points[j].y
                                     - rect_points[(j + 1) % 4].y), 2));
                    //cout << "Size:" << sizeR << endl;
                    if (sizeX == 0)
                        {
                            sizeX = sizeR;
                            shortSideMiddle.x = (rect_points[j].x
                                                 + rect_points[(j + 1) % 4].x) / 2;
                            shortSideMiddle.y = (rect_points[j].y
                                                 + rect_points[(j + 1) % 4].y) / 2;
                        }
                    else if (sizeY == 0 && sizeR != sizeX)
                        {
                            sizeY = sizeR;
                            longSideMiddle.x = (rect_points[j].x
                                                + rect_points[(j + 1) % 4].x) / 2;
                            longSideMiddle.y = (rect_points[j].y
                                                + rect_points[(j + 1) % 4].y) / 2;
                        }
                    if (picture)
                        line(out, rect_points[j], rect_points[(j + 1) % 4], Scalar(255, 0, 0));
                }
            if (sizeX > sizeY)
                {
                    Point2f temp;
                    sizeR = sizeX;
                    sizeX = sizeY;
                    sizeY = sizeR;
                    temp = longSideMiddle;
                    longSideMiddle = shortSideMiddle;
                    shortSideMiddle = temp;
                }

            rects.push_back(rect);
            PolySize polysize = { sizeX, sizeY, sizeR, shortSideMiddle, longSideMiddle };
            line_sizes.push_back(polysize);

        }
    if (picture)
        imshow("Rect", out);
}

void LineDetector::splitBigRectangles(int index)
{

    // Get the bounding rectangle
    Rect roi = boundingRect(contours_poly[index]);
    //vector<Point> contours1, contours2;
    vector<Point> contours[2];

    Point p;
    p.x = roi.x + roi.width / 2;
    vector<Point> points;
    points.push_back(p);
    Vector<RotatedRect> res = splitContourAtPoints(points, index, false);
    rects.insert(rects.end(), res.begin(), res.end());
    // Create masks for each contour to mask out that region from image.
    //Mat mask = Mat::zeros(m_frame.size(), CV_8UC1);
    //drawContours(mask, contours_poly, index, Scalar(255), CV_FILLED); // This is a OpenCV function
    /*
        for (unsigned int i = 0; i < contours_poly[index].size(); i++ )
            {
                Point p = contours_poly[index][i];
                // Separate the points into two groups
                if (p.x < (roi.x + roi.width / 2))
                    {
                        contours[0].push_back(p);
                    }
                else
                    {
                        contours[1].push_back(p);
                    }
            }

        for (unsigned int i = 0; i < 2; i++)
            {
                vector<Point> cont = contours[i];
                RotatedRect rect = minAreaRect(cont);
                Point2f rect_points[4];
                rect.points(rect_points);

                int sizeX = 0, sizeY = 0, sizeR = 0;
                Point shortSideMiddle;
                Point longSideMiddle;
                // Find rect sizes
                for (int j = 0; j < 4; j++)
                    {
                        //cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y << "]" << endl;
                        sizeR = cv::sqrt(
                                    cv::pow((rect_points[j].x - rect_points[(j + 1) % 4].x), 2)
                                    + cv::pow(
                                        (rect_points[j].y
                                         - rect_points[(j + 1) % 4].y), 2));
                        //cout << "Size:" << sizeR << endl;
                        if (sizeX == 0)
                            {
                                sizeX = sizeR;
                                shortSideMiddle.x = (rect_points[j].x
                                                     + rect_points[(j + 1) % 4].x) / 2;
                                shortSideMiddle.y = (rect_points[j].y
                                                     + rect_points[(j + 1) % 4].y) / 2;
                            }
                        else if (sizeY == 0 && sizeR != sizeX)
                            {
                                sizeY = sizeR;
                                longSideMiddle.x = (rect_points[j].x
                                                    + rect_points[(j + 1) % 4].x) / 2;
                                longSideMiddle.y = (rect_points[j].y
                                                    + rect_points[(j + 1) % 4].y) / 2;
                            }
                        line(out, rect_points[j], rect_points[(j + 1) % 4], Scalar(255, 0, 0));
                    }
                if (sizeX > sizeY)
                    {
                        Point2f temp;
                        sizeR = sizeX;
                        sizeX = sizeY;
                        sizeY = sizeR;
                        temp = longSideMiddle;
                        longSideMiddle = shortSideMiddle;
                        shortSideMiddle = temp;
                    }

                PolySize polysize = { sizeX, sizeY, sizeR, shortSideMiddle, longSideMiddle };
                rects.push_back(rect);
                line_sizes.push_back(polysize);

            }
        //drawContours(mask, contours_poly, index, Scalar(255), CV_FILLED);
        imshow("Smaller Rect", out);

    */
    //Mat region;
    //Mat imageROI;
    //m_frame.copyTo(imageROI, mask);
}

std::vector<RotatedRect>LineDetector::splitContourAtPoints(std::vector<Point> points, int contourIndex, bool yAxis)
{
    int numberOfParts = points.size() + 1;
    std::vector<Point> contours[numberOfParts];
    std::vector<RotatedRect> recs;

    bool printouts = false;

    for (unsigned int i = 0; i < contours_poly[contourIndex].size (); i++)
        {
            Point p = contours_poly[contourIndex][i];

            for (int j = 0; j < points.size (); j++)
                {
                    if (yAxis)
                        {
                            // Y axis, we expect the passed in points to be in reducing order with respect to y,i.e starting from the bottom of the screen
                            if (j == 0 && p.y >= points[j].y)
                                {
                                    contours[j].push_back (p);
                                    break;
                                }
                            else if (j == points.size() - 1 && p.y < points[j].y)
                                {
                                    contours[j + 1].push_back (p);
                                    break;
                                }
                            else if (j > 0 && p.y < points[j - 1].y && p.y > points[j].y)
                                {
                                    contours[j].push_back (p);
                                    break;
                                }
                        }
                    else
                        {
                            //X-axis,we expect the passed in points to be in increasing order with respect to x
                            if (j == 0 && p.x <= points[j].x)
                                {
                                    contours[j].push_back (p);
                                    break;
                                }
                            else if (j == points.size() - 1 && p.x > points[j].x)
                                {
                                    contours[j + 1].push_back (p);
                                    break;
                                }
                            else if (j > 0 && p.x > points[j - 1].x && p.x <= points[j].x)
                                {
                                    contours[j].push_back (p);
                                    break;
                                }
                        }
                }
        }

    for (unsigned int i = 0; i < numberOfParts; i++)
        {
            vector<Point> cont = contours[i];
            // Add points to the cut from the neighboring cuts to gain accuracy
            if (i == 0)
                {
                    if (contours[i + 1].size() > 0)
                        cont.push_back(getLowestOrHighestPoint(contours[i + 1], true));
                }
            else if (i == numberOfParts - 1)
                {
                    if (contours[i - 1].size() > 0)
                        cont.push_back(getLowestOrHighestPoint(contours[i - 1], false));
                }
            else
                {
                    if (contours[i + 1].size() > 0)
                        cont.push_back(getLowestOrHighestPoint(contours[i + 1], true));
                    if (contours[i - 1].size() > 0)
                        cont.push_back(getLowestOrHighestPoint(contours[i - 1], false));
                }

            // Check if the cut consists of enough points
            if (cont.size() < 2)
                {
                    RotatedRect rect;
                    recs.push_back(rect);
                    continue;
                }

            RotatedRect rect = minAreaRect (cont);
            PolySize polysize = createPolySize (rect);
            recs.push_back (rect);
            line_sizes.push_back (polysize);//TODO too many side effects in this function

        }
    return recs;
}

Point LineDetector::getLowestOrHighestPoint(std::vector<Point> pts, bool getLowest)
{
    Point retVal = pts[0];
    for (int i = 1; i < pts.size(); i++)
        {
            if ((getLowest && pts[i].y > retVal.y) || (!getLowest && pts[i].y < retVal.y))
                retVal = pts[i];
        }
    return retVal;
}

PolySize LineDetector::createPolySize (const RotatedRect &rect)
{
    bool printouts = false;
    if (printouts)
        cout << "__start createPolySize" << endl;
    Point2f rect_points[4];
    rect.points (rect_points);
    int sizeX = 0, sizeY = 0, sizeR = 0;
    Point shortSideMiddle;
    Point longSideMiddle;
    // Find rect sizes
    for (int j = 0; j < 4; j++)
        {
            if (printouts)
                cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y
                     << "]" << endl;
            sizeR = cv::sqrt (
                        cv::pow ((rect_points[j].x - rect_points[(j + 1) % 4].x), 2)
                        + cv::pow ((rect_points[j].y - rect_points[(j + 1) % 4].y), 2));
            //cout << "Size:" << sizeR << endl;
            if (sizeX == 0)
                {
                    sizeX = sizeR;
                    shortSideMiddle.x = (rect_points[j].x + rect_points[(j + 1) % 4].x)
                                        / 2;
                    shortSideMiddle.y = (rect_points[j].y + rect_points[(j + 1) % 4].y)
                                        / 2;
                }
            else if (sizeY == 0 && sizeR != sizeX)
                {
                    sizeY = sizeR;
                    longSideMiddle.x = (rect_points[j].x + rect_points[(j + 1) % 4].x)
                                       / 2;
                    longSideMiddle.y = (rect_points[j].y + rect_points[(j + 1) % 4].y)
                                       / 2;
                }
        }
    if (sizeX > sizeY)
        {
            Point2f temp;
            sizeR = sizeX;
            sizeX = sizeY;
            sizeY = sizeR;
            temp = longSideMiddle;
            longSideMiddle = shortSideMiddle;
            shortSideMiddle = temp;
        }

    PolySize polysize =
    { sizeX, sizeY, sizeR, shortSideMiddle, longSideMiddle };
    if (printouts)
        cout << "__end createPolySize" << endl;
    return polysize;

}


void LineDetector::classification()
{
    bool printouts = true;
    //confidenceLevel = 0;
    int sizeX;
    int sizeY;
    int sizeR;
    int area;
    RotatedRect rect;
    Point2f rect_points[4];
    Point rectCenter;
    Point shortSideMiddle;

    intersectionRect = -1;

    for (unsigned int i = 0; i < line_sizes.size(); i++)
        {
            sizeX = line_sizes[i].sizeX;
            sizeY = line_sizes[i].sizeY;
            sizeR = line_sizes[i].sizeR;
            shortSideMiddle = line_sizes[i].shortSideMiddle;
            area = sizeX * sizeY;
            rect = rects[i];
            rect.points(rect_points);
            rectCenter.x = rect.center.x;
            rectCenter.y = rect.center.y;
            rect.angle = getLineSlope(shortSideMiddle, rectCenter);

            if (printouts)
                {

                }

            if (sizeY > m_config.XTimesYMin * sizeX
                    && sizeY < m_config.XTimesYMax * sizeX
                    && sizeY < m_config.maxY)
                {
                    dashLines[cntDash] = createLineFromRect(&rect, sizeX, sizeY, i);
                    cntDash++;
                    //cout << "Dash Rect y: " << rectCenter.y << endl;
                }
            else if (sizeY > sizeX && sizeY > (m_config.maxY / 2)
                     && area < m_config.maxArea * 10000)
                {
                    solidLines[cntSolid] = createLineFromRect(&rect, sizeX, sizeY, i);
                    cntSolid++;
                }
            else if (area > m_config.maxArea * 10000)
                {
                    minXI = w;
                    minYI = h;
                    for (int j = 0; j < 4; j++)
                        {
                            if (minXI > rect_points[j].x)
                                {
                                    minXI = rect_points[j].x;
                                }
                            if (minYI > rect_points[j].y)
                                {
                                    minYI = rect_points[j].y;
                                }
                        }
                    YI = rectCenter.y;
                    if (roadState != INTERSECTION){
                        roadState = INTERSECTION_AHEAD;
                    }
                    intersectionRect = i;

                    //intersectionOn = true;
                    //foundIntersection = true;
                    float angle_thr = 10;
                    float height_thr = (2*h)/3;
                    cout << "Possible INTERSECTION"
                         << "\tCenter: " << rectCenter.y << "ANgle: " << rect.angle
                         << endl;
                    //confidenceLevel = 2;
                    //roadState = INTERSECTION;rect.angle

                    cout << "(abs(rect.angle) < angle_thr || 180 - abs(rect.angle) < angle_thr ) && rectCenter.y > height_thr && roadState == INTERSECTION_AHEAD" << endl;
                    cout << "(" << abs(rect.angle) <<" <(?) " << angle_thr << " || "<< 180 - abs(rect.angle) << " <(?) " << angle_thr<< ") && " << rectCenter.y << " >(?) " << height_thr << "&& roadState: " << roadState << endl;
                    
                    if ( (abs(rect.angle) < angle_thr || 180 - abs(rect.angle) < angle_thr ) && rectCenter.y > height_thr && roadState == INTERSECTION_AHEAD)
                        {
                            roadState = INTERSECTION;
                            //confidenceLevel = CONFIDENCE_LEVEL_MAX;
                            intersectionOn = true;
                            foundIntersection = true;
                            cout << "INTERSECTION   !!!!!!!!!!!!" << endl;
                            if (m_debug)
                                {
                                    cout << "Intersection x: " << minXI << ", Intersection y: " << minYI
                                         << "\n Angle: " << rect.angle << "Center y: " << rectCenter.y << endl;
                                }
                        }
                    //////////////////////////////////////////////
                    //New intersection handling when there is a stop line
                    //1.We check the angle of the rect
                    //we convert the angle so that we always measure the angle between the the vertical line and the longest side
                    //of the the rectangle.In this case when the long side is laying flat then the angle is 90 degress if the shorter
                    //side is the one laying flat then its 0
                    /*
                    float angle = 0;
                    if (rect.size.width < rect.size.height)
                    {
                    angle = rect.angle + 180;
                          }
                        else
                          {
                      angle = rect.angle + 90;
                          }

                        if (angle > 80.0 && angle < 110.0)
                          {
                      confidenceLevel=1;
                      //investigate further
                      //if we have object avoidance we check how much white is in this rectangle
                      //otherwise we assume is has to be an intersection we then determine how far off we are
                      if (rect.size.width > w / 2 || rect.size.height > w / 2)
                        {      //we also check for height as rotated rect has some
                          //weird map of what width and what's height depending on the orientation
                          confidenceLevel=2;
                          //I think intersection detection should be stateful and not frame by frame
                          roadState=INTERSECTION;
                          if(m_debug){
                          cout << "INTERSECTION POTENTIAL!" << endl;
                          cout << "ANGLE OF RECT " << angle << endl;
                          cout << "Width " << rect.size.width << endl;
                          cout << "height " << rect.size.height << endl;
                          }
                          float intersecRectLimit=255;//we need to tweak this appropriately,maybe make it a screen config variable
                          cout<<endl;
                          Point2f center=rect.center;
                          if(center.y >intersecRectLimit){
                          if(m_debug)
                            cout<<"INTERSECTION NOW"<<center<<endl;
                          confidenceLevel=CONFIDENCE_LEVEL_MAX;//We are very sure!!
                          }

                        }
                          }
                        ///////////////////////////////////////////////////////////////////////////////////
                      */

                }
        }

    //////////////////
    // Check if we have entered the intersection
    //////////////////
    // if (roadState == INTERSECTION_AHEAD && intersectionRect == -1){
    //     roadState = INTERSECTION;
    // }

    if (intersectionOn && !foundIntersection)
        {
            YI = h;
        }
}

void LineDetector::filterAndMerge()
{
    for (int j = 0; j < cntSolid; j++)
        {
            float a = tan(M_PI * solidLines[j].slope / 180);
            Point center;
            center.x = (solidLines[j].p1.x + solidLines[j].p2.x) / 2;
            center.y = (solidLines[j].p1.y + solidLines[j].p2.y) / 2;
            float b = center.y - center.x * a;
            //cout << "Equation [a,b]: [" << a << "," << b << "]" << endl;
            //cout << "Dashes" << endl;
            if ((solidLines[j].slope > MIN_ANGLE - 5
                    && max(solidLines[j].p1.x, solidLines[j].p2.x) > w / 2)
                    || (solidLines[j].slope < (-1) * (MIN_ANGLE - 5)
                        && min(solidLines[j].p1.x, solidLines[j].p2.x) < w / 2))
                {
                    for (int l = 0; l < cntDash; l++)
                        {
                            Point dashCenter;
                            dashCenter.x = (dashLines[l].p1.x + dashLines[l].p2.x) / 2;
                            dashCenter.y = (dashLines[l].p1.y + dashLines[l].p2.y) / 2;
                            float res = a * dashCenter.x + b;
                            //cout << "[res, y] = [" << res << "," << dashCenter.y << "]" << endl;
                            //cout << "[x, y] = [" << dashCenter.x << "," << dashCenter.y << "]" << endl;
                            if (res > dashCenter.y)
                                {
                                    dashLines[l] = dashLines[cntDash - 1];
                                    cntDash--;
                                    l--;
                                    //cout<< cntDash <<endl;
                                }
                        }
                    //cout << "Solids" << endl;
                    for (int k = j + 1; k < cntSolid; k++)
                        {
                            Point sldCenter;
                            sldCenter.x = (solidLines[k].p1.x + solidLines[k].p2.x) / 2;
                            sldCenter.y = (solidLines[k].p1.y + solidLines[k].p2.y) / 2;
                            float res = a * sldCenter.x + b;
                            if (res > sldCenter.y)
                                {
                                    solidLines[k] = solidLines[cntSolid - 1];
                                    cntSolid--;
                                    k--;
                                    //cout<< cntSolid <<endl;
                                }
                        }
                }
        }
}

void LineDetector::finalFilter()
{
    for (int i = 0; i < cntSolid; i++)
        {
            CustomLine l = solidLines[i];
            int minAngle = MIN_ANGLE - 5;
            //cout << "Slope: " << l.slope << " min is " << minAngle << endl;
            if (abs(l.slope) < minAngle)
                {
                    solidLines[i] = solidLines[cntSolid - 1];
                    cntSolid--;
                    if (i > 0)
                        {
                            i--;
                        }
                    foundStopStartLine = true;
                }
        }

    //Dash also positioned too high on the image or too left or too right
    int maxDashY = 0;
    for (int i = 0; i < cntDash; i++)
        {
            CustomLine l = dashLines[i];
            int dashCenterX = (l.p1.x + l.p2.x) / 2;
            int dashCenterY = (l.p1.y + l.p2.y) / 2;
            //cout << "Slope: " << l.slope << " min is " << MIN_ANGLE << endl;
            if ((l.slope < MIN_ANGLE && l.slope > ((-1) * MIN_ANGLE))
                    || (dashCenterY < h / 15) || (dashCenterX > 19 * w / 20)) //|| (dashCenterX < w/20) too left //too high
                {
                    dashLines[i] = dashLines[cntDash - 1];
                    cntDash--;
                    if (i > 0)
                        {
                            i--;
                        }
                }

            if (maxDashY < max(dashLines[i].p1.y, dashLines[i].p2.y))
                {
                    maxDashY = max(dashLines[i].p1.y, dashLines[i].p2.y);
                }
        }
    // This check is now made in the lower part of characteristicFiltering(..)
    //cout << "MaxDashY"    << maxDashY
    //      << "\nScreen section" << (9 * h / 10) << endl;
    //if ((cntSolid > 0 && cntDash > 1 && maxDashY < (9 * h / 10)) || YI < 120)
     // if (cntSolid > 0 && cntDash > 1 && maxDashY > (9 * h / 10) && roadState== INTERSECTION)
     //    {
     //         cout << "=========================================================================================="
     //              <<  "Intersection off "
     //              << "MaxDashY"   << maxDashY  << endl;
     //         intersectionOn = false;
     //         roadState = NORMAL;
     //     }
}

void LineDetector::characteristicFiltering(LinesToUse *ltu)
{
    bool printouts = true;
    // Now we got the lines which we actually shall work with
    if (printouts)
        {
            cout << "__start characteristicFiltering" << endl;
            cout << "currentLeftGoalX: " << currentLeftGoalX << endl;
            cout << "currentDashGoalX: " << currentDashGoalX << endl;
            cout << "currentRightGoalX: " << currentRightGoalX << endl;
            cout << "calcRoadSize: " << calcRoadSize << endl;
            cout << "intersectionOn: " << intersectionOn << endl;
            cout << "roadState: " << roadState << endl;
        }

    //LinesToUse old_ltu;
    // if (ltu != NULL)
    //  old_ltu = *ltu;

    //ofstream mylog;
    //mylog.open("test.log", ios::out | ios::app);

    ltu->foundR = false;
    ltu->foundL = false;
    ltu->foundD = false; // shrinkSize=false; Avoid unused variable warning

    ltu->dashLineVec = Vec4i(0, 0, 0, 0);
    ltu->leftLineVec = Vec4i(0, 0, 0, 0);
    ltu->rightLineVec = Vec4i(0, 0, 0, 0);
    //Pick the suitable dashLine
    if (printouts)
        cout << "cntDash: " << cntDash << endl;
    ltu->dashedCurveFound = false;
    if (cntDash > 0)
        {
            ltu->dashLine.p1.y = 0;
            ltu->dashLine.p2.y = 0;
            // Sort the dashes by highest p1.y value first
            std::sort(dashLines.begin(), dashLines.begin() + cntDash);
            cout << endl;
            if (printouts)
                {
                    for (int j = 0; j < cntDash; j++)
                        {
                            cout << "Dash line. p1(" << dashLines[j].p1.x << "," << dashLines[j].p1.y << ") p2(" << dashLines[j].p2.x << "," << dashLines[j].p2.y << ")" << endl;
                        }
                }
            ltu->cntDash = cntDash;
            // Make p1 be the bottom point
            for (int i = 0; i < cntDash; i++)
                {
                    if (dashLines[i].p1.y < dashLines[i].p2.y)
                        {
                            // Flipping p1 <-> p2, recalc slope.
                            if (printouts)
                                cout << "Flipping p1 <-> p2, recalc slope" << endl;
                            Point tmp = dashLines[i].p1;
                            dashLines[i].p2 = dashLines[i].p1;
                            dashLines[i].p2 = tmp;
                            dashLines[i].slope = getLineSlope(dashLines[i].p1, dashLines[i].p2);
                        }
                }
            if (printouts)
                cout << "---Start dash Lines" << endl;
            // Try to find the dashed curve composed of several dashes
            //
            // TODO: it now assumes that the dashed curves are completely independent;
            // that a curve can not split into more curves. Right now it just picks the
            // first dash that matches and append that to the found curve, it does not
            // continue to look if other dashes also matches. (findCurve should spawn
            // one CustomLine vector for eash found curve)
            //
            if (cntDash > 1)
                {
                    // Copy needed data
                    std::vector<CustomLine> dashedLines = dashLines;
                    std::vector<CustomLine> unusedLines;
                    int cntDashed = cntDash;

                    // The found curves will be stored in this
                    std::vector<vector<CustomLine> > curves;

                    for (int i = 0; i < cntDash - 1; i++)
                        {
                            // When fiding curve from i, dashes present at i < don't need to be checked.
                            std::vector<CustomLine> lines;
                            for (int j = 0; j < cntDashed; j++)
                                {
                                    lines.push_back(dashedLines[j]);
                                }

                            std::vector<CustomLine> res = findCurve(lines);
                            if (res.size() == 0)
                                {
                                    unusedLines.push_back(dashedLines[0]);
                                    dashedLines.erase(dashedLines.begin());
                                    cntDashed--;
                                    continue;
                                }
                            // If curve found, we proceed
                            std::vector<CustomLine> curve;
                            curve.push_back(lines[0]);

                            for (int j = 0; j < res.size(); j++)
                                {
                                    curve.push_back(res[j]);
                                }
                            // Save the found curve
                            curves.push_back(curve);

                            // Remove dashed lines already a part of a found curve
                            for (int j = 0; j < curve.size(); j++)
                                {
                                    for (int k = 0; k < cntDashed; k++)
                                        {
                                            if (curve[j] == dashedLines[k])
                                                {
                                                    if (printouts)
                                                        cout << "remove used dash: p1(" << curve[j].p1.x << "," << curve[j].p1.y << ") p2(" << curve[j].p2.x << "," << curve[j].p2.y << ") " << endl;
                                                    dashedLines.erase(dashedLines.begin() + k);
                                                    cntDashed--;
                                                }
                                        }
                                }
                            // Check if we got enough lines to make another curve
                            if (cntDashed < 2)
                                {
                                    if (printouts)
                                        cout << "less then two dashed lines left to use, breaks loop." << endl;
                                    break;
                                }
                        }
                    // Pick which curve to use
                    // Now it simple picks the (first found) longest one
                    int longest_curve = 0;
                    for (int i = 0; i < curves.size(); i++)
                        {
                            ltu->dashedCurveFound = true;

                            if (curves[i].size() > longest_curve)
                                {
                                    dashLines[0] = curves[i][0];
                                    ltu->dashedCurve = curves[i];
                                }
                            if (printouts)
                                {
                                    // Print the found curve
                                    if (printouts)
                                        {
                                            cout << "size: " << curves[i].size() << " Dashed curve is: ";
                                            for (int j = 0; j < curves[i].size(); j++)
                                                {
                                                    cout << "p1(" << curves[i][j].p1.x << "," << curves[i][j].p1.y << ") ";
                                                    cout << "p2(" << curves[i][j].p2.x << "," << curves[i][j].p2.y << ") " << endl;
                                                }
                                        }
                                }
                        }
                    global_dashedCurve = ltu->dashedCurve;
                    // Check if any remaining dashed lines not a part of a curve could be potential
                    // left or right lines.
                    if (ltu->dashedCurveFound)
                        {
                            for (int j = 0; j < cntDashed; j++)
                                {
                                    unusedLines.push_back(dashedLines[j]);
                                }
                            if (printouts)
                                {
                                    cout << "unusedLines: " << unusedLines.size() << endl;
                                    cout << "Curve p1.y(" << ltu->dashedCurve[0].p1.y << ") p2.y(" << ltu->dashedCurve[ltu->dashedCurve.size() - 1].p2.y << ") " << endl;
                                }

                            for (int i = 0; i < unusedLines.size(); i++)
                                {
                                    int s = ltu->dashedCurve.size() - 1;

                                    // Checks if a unused dash line is interleaved with the found dashed curve w.r.t the y-axis
                                    if (!(((unusedLines[i].p1.y > ltu->dashedCurve[0].p1.y) &&
                                            (unusedLines[i].p2.y > ltu->dashedCurve[0].p1.y)) ||
                                            ((unusedLines[i].p1.y < ltu->dashedCurve[s].p2.y) &&
                                             (unusedLines[i].p2.y < ltu->dashedCurve[s].p2.y))))
                                        {
                                            // Filter away short lines that probably do not have an accurate angle
                                            if (getDist(unusedLines[i].p1, unusedLines[i].p2) > (m_config.maxY / 3)){
                                                solidLines[cntSolid] = unusedLines[i];
                                                cntSolid++;
                                                if (printouts)
                                                    cout << "Added to solid: p1(" << unusedLines[i].p1.x << "," << unusedLines[i].p1.y << ") p2(" << unusedLines[i].p2.x << "," << unusedLines[i].p2.y << ") " << endl;
                                            }else{
                                                if (printouts){
                                                    cout << "getDist(unusedLines[i].p1, unusedLines[i].p2): " << getDist(unusedLines[i].p1, unusedLines[i].p2) << "(m_config.maxY / 3): " << (m_config.maxY / 3) << endl;
                                                }
                                            }
                                        }
                                    else if (printouts)
                                        cout << "Line not added p1(" << unusedLines[i].p1.x << "," << unusedLines[i].p1.y << ") p2(" << unusedLines[i].p2.x << "," << unusedLines[i].p2.y << ") " << endl;
                                }
                            // set currentDashGoalX
                            ltu->dashLine = ltu->dashedCurve[0];
                            if (printouts)
                                cout << "Dash diff: " << abs(getIntersectionWithBottom(ltu->dashLine) - currentDashGoalX) << " <? " << calcRoadSize * 0.8 << endl;
                            currentDashGoalX = getIntersectionWithBottom(ltu->dashLine);
                            ltu->dashLineVec = Vec4i(ltu->dashLine.p1.x, ltu->dashLine.p1.y, ltu->dashLine.p2.x, ltu->dashLine.p2.y);
                            ltu->foundD = true;
                            if (printouts)
                                cout << "Dash chosen: p1(" << ltu->dashLine.p1.x << "," << ltu->dashLine.p1.y << ") p2(" << ltu->dashLine.p2.x << "," << ltu->dashLine.p2.y << ")" << endl;
                        }
                }

            //
            // The old way to do it follows
            //
            if (!ltu->dashedCurveFound)
                {
                    for (int i = 0; i < cntDash; i++)
                        {
                            cout << "new iteration" << endl;
                            for (int j = 0; j < cntDash; j++)
                                {
                                    cout << "Dash line. p1(" << dashLines[j].p1.x << "," << dashLines[j].p1.y << ") p2(" << dashLines[j].p2.x << "," << dashLines[j].p2.y << ")" << endl;
                                }
                            //cout << "Dash y: " << max(dashLines[i].p1.y,dashLines[i].p2.y) << endl;
                            //cout << "Dash max: " << max(dashLines[i+1].p1.y,dashLines[i+1].p2.y) << " Dash min: " <<  min(dashLines[i].p1.y,dashLines[i].p2.y) << endl;
                            if (i != cntDash - 1 && max(dashLines[i + 1].p1.y, dashLines[i + 1].p2.y)
                                    > min(dashLines[i].p1.y, dashLines[i].p2.y))
                                {
                                    //cout << "Removing wrong dash!" << endl;
                                    int positionX = getIntersectionWithBottom(dashLines[i]);
                                    int nPositionX = getIntersectionWithBottom(dashLines[i + 1]);
                                    if (printouts)
                                        {
                                            cout << "current closest: p1(" << dashLines[i].p1.x << "," << dashLines[i].p1.y << ") p2(" << dashLines[i].p2.x << "," << dashLines[i].p2.y << ")" << endl;
                                            cout << "other: p1(" << dashLines[i + 1].p1.x << "," << dashLines[i + 1].p1.y << ") p2(" << dashLines[i + 1].p2.x << "," << dashLines[i + 1].p2.y << ")" << endl;
                                            cout << "bottomX Curr closest: " << positionX << ", other: " << nPositionX << endl;
                                            cout << "bottomX abs Curr closest: " << abs(currentDashGoalX - positionX) << ", other: " << abs(currentDashGoalX - nPositionX) << endl;
                                        }
                                    if (abs(currentDashGoalX - positionX)
                                            < abs(currentDashGoalX - nPositionX))
                                        {
                                            dashLines.erase(dashLines.begin() + 1);
                                            cntDash--;
                                            cout << "rm other" << endl;
                                        }
                                    else
                                        {
                                            dashLines.erase(dashLines.begin());
                                            cntDash--;
                                            cout << "rm current" << endl;
                                        }
                                    if (i > 0)
                                        {
                                            i--;
                                        }
                                }

                            if (intersectionOn && ((dashLines[i].p1.y + dashLines[i].p2.y) / 2) < YI)
                                {
                                    dashLines[i] = dashLines[cntDash - 1];
                                    cntDash--;
                                    if (i > 0)
                                        {
                                            i--;
                                        }
                                    if (m_debug)
                                        {
                                            cout << "Remove dash becuase of intersection! (" << YI
                                                 << ") " << endl;
                                        }
                                }
                        }
                    if (cntDash > 0)
                        {
                            ltu->dashLine = dashLines[0];
                            int dashSupPosX = getIntersectionWithBottom(ltu->dashLine);
                            //if(ltu->dashLine.slope < 0) {
                            if (printouts)
                                {
                                    cout << "Dash line slope: " << ltu->dashLine.slope << endl;
                                    cout << "Dash diff: " << abs(dashSupPosX - currentDashGoalX) << " <? " << calcRoadSize * 0.8 << endl;
                                }
                            if (abs(dashSupPosX - currentDashGoalX) < calcRoadSize * 0.8
                                    || currentDashGoalX == 0)
                                {
                                    /*if(max(ltu->dashLine.p1.x, ltu->dashLine.p2.x) < w/10) {
                                     shrinkSize = true;
                                     }*/
                                    ltu->dashLineVec = Vec4i(ltu->dashLine.p1.x, ltu->dashLine.p1.y,
                                                             ltu->dashLine.p2.x, ltu->dashLine.p2.y);
                                    ltu->foundD = true;
                                    if (printouts)
                                        cout << "Dash chosen: p1(" << dashLines[0].p1.x << "," << dashLines[0].p1.y << ") p2(" << dashLines[0].p2.x << "," << dashLines[0].p2.y << ")" << endl;
                                    currentDashGoalX = dashSupPosX;
                                }
                        }
                    ltu->dashedCurve.push_back(dashLines[0]);

                }
        }
    if (printouts)
        {
            cout << "---End dash Lines" << endl;
            cout << "---Start right Lines" << endl;
            for (int i = 0; i < cntSolid; i++)
                {
                    cout << "solid line " << i << ": p1(" << solidLines[i].p1.x << "," << solidLines[i].p1.y << ") p2(" << solidLines[i].p2.x << "," << solidLines[i].p2.y << ") slope: " << solidLines[i].slope << endl;
                }
        }
    // Determine which solid line is the left and right solid lines
    if (cntSolid > 0 && !intersectionOn)
        {
            ltu->rightLine.p1.x = w + 50;
            ltu->rightLine.p2.x = w + 50;
            for (int i = 0; i < cntSolid; i++)
                {
                    if (solidLines[i].slope < 90 && solidLines[i].slope > 0
                            && min(solidLines[i].p1.x, solidLines[i].p2.x)
                            < min(ltu->rightLine.p1.x, ltu->rightLine.p2.x))
                        {
                            if (printouts)
                                cout << "solid line " << i << " aspires as right line" << endl;
                            ltu->rightLine = solidLines[i];
                            ltu->foundR = true;
                        }
                }
            if (ltu->foundR)
                {
                    int rSupPosX = getIntersectionWithBottom(ltu->rightLine);
                    if (printouts)
                        {
                            cout << "right getIntersectionWithBottom: " << rSupPosX << endl;
                            cout << "Right diff: " << abs(rSupPosX - currentRightGoalX) << " <? " << calcRoadSize * 0.8 << endl;
                        }
                    if (abs(rSupPosX - currentRightGoalX) < calcRoadSize * 0.8
                            || currentRightGoalX == 0)
                        {
                            if (printouts)
                                {
                                    cout << "Right line slope: " << ltu->rightLine.slope
                                         << endl;
                                }
                            ltu->rightLineVec = Vec4i(ltu->rightLine.p1.x, ltu->rightLine.p1.y,
                                                      ltu->rightLine.p2.x, ltu->rightLine.p2.y);
                            if (printouts)
                                cout << "Aspired right line chosen" << endl;
                            currentRightGoalX = rSupPosX;
                        }
                    else
                        {
                            ltu->foundR = false;
                        }
                }
            if (printouts)
                {
                    cout << "---End right Lines" << endl;
                    cout << "---Start left Lines" << endl;
                }
            ltu->leftLine.p1.x = -50;
            ltu->leftLine.p2.x = -50;
            for (int i = 0; i < cntSolid; i++)
                {
                    // centerSolidLineX commented to avoid unused variable warning!
                    //int centerSolidLineX = (solidLines[i].p1.x + solidLines[i].p2.x)/2;
                    if (solidLines[i].slope > -90 && solidLines[i].slope < 0
                            && min(solidLines[i].p1.x, solidLines[i].p2.x)
                            > min(ltu->leftLine.p1.x, ltu->leftLine.p2.x))
                        {
                            if (printouts)
                                cout << "solid line " << i << " aspires as left line" << endl;
                            ltu->leftLine = solidLines[i];
                            ltu->foundL = true;
                        }
                }
            if (ltu->foundL)
                {
                    int lSupPosX = getIntersectionWithBottom(ltu->leftLine);
                    if (printouts)
                        {
                            cout << "left getIntersectionWithBottom: " << lSupPosX << endl;
                            cout << "Left diff: " << abs(lSupPosX - currentLeftGoalX) << " <? " << calcRoadSize * 0.8 << endl;
                        }
                    if (abs(lSupPosX - currentLeftGoalX) < calcRoadSize * 0.8
                            || currentLeftGoalX == 0)
                        {
                            if (printouts)
                                {
                                    cout << "Left line slope: " << ltu->leftLine.slope
                                         << endl;
                                }
                            ltu->leftLineVec = Vec4i(ltu->leftLine.p1.x, ltu->leftLine.p1.y,
                                                     ltu->leftLine.p2.x, ltu->leftLine.p2.y);
                            if (printouts)
                                cout << "Aspired left line chosen" << endl;
                            currentLeftGoalX = lSupPosX;
                        }
                    else
                        {
                            ltu->foundL = false;
                        }
                }
        }

    // Get rid of information gathered one frame back
    // if (!ltu->foundD)
    //     currentDashGoalX = 0;
    // if (!ltu->foundR)
    //     currentRightGoalX = 0;
    // if (!ltu->foundL)
    //     currentLeftGoalX = 0;

    if (printouts)
        {
            cout << "---End left Lines" << endl;
            cout << "currentLeftGoalX: " << currentLeftGoalX << endl;
            cout << "currentDashGoalX: " << currentDashGoalX << endl;
            cout << "currentRightGoalX: " << currentRightGoalX << endl;
        }

    //////////////////
    // This check is used to find out if we have passed an intersection.
    // If it is passed, it will set the roadState to NORMAL.
    //////////////////
    cout << "_:_intersection checks start" << endl;
    cout << "intersectionRect: " << intersectionRect << endl;
    cout << "roadState: " << roadState << endl;
    cout << "YI: " << YI << endl;
    if (intersectionRect != -1){
        cout << "intRect.angle: " << rects[intersectionRect].angle << endl;        
    }
    
    if(roadState == INTERSECTION){
        if (printouts)
            cout << "entering passed intersection check" << endl;
        int maxY = 0;
        for (int i = 0; i < ltu->dashedCurve.size(); i ++){
            if (maxY < ltu->dashedCurve[i].p1.y){ // p1 is always the point furtest down on the frame
                maxY = ltu->dashedCurve[i].p1.y;
            }
        }
        if (printouts){
            cout << "maxY: " << maxY << " > " << (8 * h / 10) << endl;
            cout << "ltu->dashedCurve.size(): " << ltu->dashedCurve.size() << endl;
        }
        if (ltu->dashedCurve.size() > 1 && maxY > (8 * h / 10)){
            int minSlope = 40;
            if (printouts){
                cout << "abs(ltu->dashedCurve[0].slope) > minSlope  " << abs(ltu->dashedCurve[0].slope) << ">(?)" << minSlope  << endl;
            }
            if (abs(ltu->dashedCurve[0].slope) > minSlope ){
                if (printouts)
                    cout << "roadState set to NORMAL" << endl;
                roadState = NORMAL;
                intersectionOn = false;
                foundIntersection = false;

            }
        }
        cout << "leaving passed intersection check" << endl;
    }
    cout << "_:_intersection checks end" << endl;

    if (printouts)
        {
            cout << "__end characteristicFiltering" << endl;
        }
    return;
}

void LineDetector::createTrajectory(LinesToUse *ltu)
{
    bool printouts = false;
    if (printouts)
        cout << "__start createTrajectory" << endl;

    // The found lines are used to create a trajectory for the car's future movement

    if (!(ltu->foundL || ltu->foundD || ltu->foundR))
        {
            cout << "No lines found, trajectory will not be derived." << endl;

            finalOutput.noTrajectory = true;
            dataToDriver = new LaneDetectorDataToDriver(); // Empty call will set noTrajectory = true
            return;
        }
    std::vector<int> defaultCutPoints;
    defaultCutPoints.push_back(180);
    defaultCutPoints.push_back(100);
    defaultCutPoints.push_back(50);
    std::vector<int> cutPoints;
    std::vector<CustomLine> leftSplitted;
    std::vector<CustomLine> rightSplitted;
    std::vector<CustomLine> dashToUse;

    //////////////////
    // The code (commented out) below provides at least two cut points up to as many cut points as there is dashes found.
    // The function also stores the found dashes in the vector dashToUse that is later used as input to goalLine calculations.
    // If no dashes are found, the default cut points are used and dashToUse is filled with dummy lines.
    //////////////////
    // -- Derive cutpoints for the solid line and match which dash to use to which (arbitrary many goal lines)
    // part of the cutted solid line --
    // if (ltu->foundD)
    //     {
    //         dashToUse = ltu->dashedCurve;
    //         // Set up cut points for splitting it and specify where the goal lines shall start at
    //         for (int i = 0; i < ltu->dashedCurve.size(); i++)
    //             {
    //                 int cutP = ltu->dashedCurve[i].p2.y;
    //                 if (cutP > defaultCutPoints[0])
    //                     cutPoints.push_back(defaultCutPoints[0]);
    //                 else
    //                     cutPoints.push_back(cutP);
    //             }

    //         if (ltu->foundR || ltu->foundL)
    //             // Add cut points to have cut points throughout the whole frame
    //             {
    //                 int lowestDashPointInLowestCut = ltu->dashedCurve[0].p1.y; // Observe that p1 is used
    //                 int highestCut = cutPoints[cutPoints.size() - 1];

    //                 for (int i = 0; i < defaultCutPoints.size(); i++)
    //                     {
    //                         if (highestCut - 50 > defaultCutPoints[i])
    //                             {
    //                                 cutPoints.push_back(defaultCutPoints[i]);
    //                                 dashToUse.push_back(getNoneCustomLine());
    //                             }
    //                         else if (lowestDashPointInLowestCut + 30 < defaultCutPoints[i])
    //                             {
    //                                 cutPoints.insert(cutPoints.begin(), defaultCutPoints[i]);
    //                                 // It is assumed that it is safe to use the same dash line eq. to
    //                                 // calculate the goalLine more closer to the car.
    //                                 // TODO: Verify assumption.
    //                                 dashToUse.insert(dashToUse.begin(), CustomLine(dashToUse[0]));
    //                             }
    //                     }
    //                 dashToUse.push_back(getNoneCustomLine());
    //             }
    //     }
    // else if (ltu->foundR || ltu->foundL)
    //     // If we got no dash lines but we got a solid, provide cut points for spliting the solid line.
    //     {
    //         cutPoints = defaultCutPoints;
    //         for (int i = 0; i < defaultCutPoints.size() + 1; i++)
    //             {
    //                 dashToUse.push_back(getNoneCustomLine());
    //             }
    //     }

    //////////////////
    // The code below is a REALLY simple version of the code above; this provides maximum of one cut point.
    // The function also stores maximum two found dashes in the vector dashToUse that is later used as input to goalLine calculations.
    // If no dashes are found, the default cut points are used and dashToUse is filled with dummy lines.
    // This code result in that createTrajectory(..) will output at most two goalLines.
    //////////////////
    // if (ltu->foundD)
    //     {
    //         dashToUse.push_back((ltu->dashedCurve)[0]);
    //         int cutP = ltu->dashedCurve[0].p2.y;
    //         if (cutP > defaultCutPoints[0])
    //             cutPoints.push_back(defaultCutPoints[0]);
    //         else
    //             cutPoints.push_back(cutP);

    //         if (ltu->dashedCurve.size() > 1)
    //                 dashToUse.push_back((ltu->dashedCurve)[1]);
    //         else
    //                 dashToUse.push_back(getNoneCustomLine());
    //     }
    // else
    //     {
    //         cutPoints.push_back(defaultCutPoints[0]);
    //         dashToUse.push_back(getNoneCustomLine());
    //         dashToUse.push_back(getNoneCustomLine());
    //     }

    //////////////////
    // The code below splits the found solid lines at the derived cut points.
    // If the solid line was not found, dummy lines (None lines) is inserted.
    //////////////////
    // bool splitRight = false;
    // if (ltu->foundR || ltu->foundL)
    //     {
    //         // Prioritize to split right line if it is found
    //         if (ltu->foundR)
    //             {
    //                 splitRight = true;
    //                 rightSplitted = splitSolidLines(cutPoints, ltu->rightLine);
    //             }

    //         if (ltu->foundL && (!splitRight || true))
    //             // When two goalLines is wanted, change last bool to true
    //             {
    //                 leftSplitted = splitSolidLines(cutPoints, ltu->leftLine);
    //             }
    //     }
    // // Fill potentially empty vector with None lines
    // if (leftSplitted.size() == 0)
    //     {
    //         for (int i = 0; i < dashToUse.size(); i++)
    //             {
    //                 leftSplitted.push_back(getNoneCustomLine());
    //             }
    //     }
    // if (rightSplitted.size() == 0)
    //     {
    //         for (int i = 0; i < dashToUse.size(); i++)
    //             {
    //                 rightSplitted.push_back(getNoneCustomLine());
    //             }
    //     }

    //////////////////
    // This code is used for the simplest option possible and will not cut any solid line
    // and it will provide maximum one goalLine
    //////////////////
    if (ltu->foundR)
        rightSplitted.push_back(ltu->rightLine);
    else
        rightSplitted.push_back(getNoneCustomLine());

    if (ltu->foundL)
        leftSplitted.push_back(ltu->leftLine);
    else
        leftSplitted.push_back(getNoneCustomLine());

    if (ltu->foundD)
        dashToUse.push_back((ltu->dashedCurve)[0]);
    else
        dashToUse.push_back(getNoneCustomLine());
    cutPoints.push_back(h);
    //////////////////
    // End simplest option code
    //////////////////

    if (printouts)
        cout << "rightSplitted: " << rightSplitted.size() << " leftSplitted: " << leftSplitted.size() << " dashToUse.size(): " << dashToUse.size() << endl;
    // used for debug of getRoadSize and getRoadAngle
    rrd.nmbOfRounds = 0;

    //////////////////
    // This for loop creates the goalLines, one for each iteration.
    // The three lines (left, dash, right) associated to a specific cut is fed to provideGoalLine(..).
    // Remember that those can potentially be dummy lines.
    //////////////////
    std::vector<CustomLine> rightGoalLines, leftGoalLines;
    std::vector<bool> estimatedLeft (dashToUse.size(), false);
    std::vector<bool> estimatedDash (dashToUse.size(), false);
    std::vector<bool> estimatedRight (dashToUse.size(), false);
    std::vector<int> confidenceLevel_goalLine;
    for (int i = 0; i < dashToUse.size(); i++)
        {
            if (printouts)
                cout << "------calcgoalline start lap: " << i << endl;
            EstimationData ed;
            GoalLineData gld;
            ed.left = leftSplitted[i];
            ed.dash = dashToUse[i];
            ed.right = rightSplitted[i];
            // ed.left = getNoneCustomLine();
            // ed.dash = getNoneCustomLine();
            // ed.right = getNoneCustomLine();
            if (i == 0)
                ed.yPosition = h;
            else
                ed.yPosition = cutPoints[i - 1];

            provideGoalLine(&ed, &gld);
            if (m_debug)
                {
                    // This is done to display the estimated lines properly
                    if (ed.isLeftEstimated)
                        {
                            estimatedLeft[i] = true;
                            ed.left.p2.x = getIntersectionWithTop(ed.left);
                            leftSplitted[i] = ed.left;
                        }
                    if (ed.isDashEstimated)
                        {
                            estimatedDash[i] = true;
                            ed.dash.p2.x = getIntersectionWithTop(ed.dash);
                            dashToUse[i] = ed.dash;
                        }
                    if (ed.isRightEstimated)
                        {
                            estimatedRight[i] = true;
                            ed.right.p2.x = getIntersectionWithTop(ed.right);
                            rightSplitted[i] = ed.right;
                        }
                }
            confidenceLevel_goalLine.push_back(gld.confidenceLevel_rightGoalLine);
            rightGoalLines.push_back(gld.rightGoalLine);
            leftGoalLines.push_back(gld.leftGoalLine);
        }
    int confidenceLevel_goalLine0 = 0;
    if (rightGoalLines.size() > 0)
        {
            confidenceLevel_goalLine0 = confidenceLevel_goalLine[0];
        }


    // used for debug of getRoadSize and getRoadAngle
    // Mat frame = m_frame_color.clone();
    // for (int i = 0; i < rrd.lineUsed.size(); i++){
    //     line(frame, rrd.lineUsed[i].p1, rrd.lineUsed[i].p2, Scalar(255, 0, 0), 2, CV_AA);

    //     CustomLine a; // the road angle is displayed with this line
    //     a.slope = rrd.roadAngle[i];
    //     a.p1.x = w/2;
    //     a.p1.y = rrd.yPosition[i];
    //     a.p2.x = getIntersectionWithTop(a);
    //     a.p2.y = 0;
    //     line(frame, a.p1, a.p2, Scalar(0, 255, 0), 2, CV_AA);

    //     if (rrd.whichLine[i] != -1){
    //         CustomLine s; // the road size is displayed with this line
    //         s.p1.y = rrd.yPosition[i];
    //         s.p2.y = rrd.yPosition[i];
    //         s.p1.x = rrd.lineUsed[i].p1.x;
    //         if(rrd.whichLine[i] == 0) // Right is 0
    //             s.p2.x = s.p1.x - rrd.roadSize[i];
    //         else if (rrd.whichLine[i] == 1)
    //             s.p2.x = s.p1.x + rrd.roadSize[i];
    //         else if (rrd.whichLine[i] == 2)
    //             s.p2.x = s.p1.x + rrd.roadSize[i];

    //         line(frame, s.p1, s.p2, Scalar(0, 0, 255), 2, CV_AA);
    //     }
    // }
    // imshow("used for debug of getRoadSize and getRoadAngle", frame);

    //////////////////
    // Here is the currentLine derived, which states the car's current position and heading.
    //////////////////
    Point position;
    position.x = w / 2;
    position.y = h;
    Point heading;
    heading.x = w / 2;
    heading.y = 0;
    //Create car orientation vector
    CustomLine currentLine;
    currentLine.p1 = heading;
    currentLine.p2 = position;
    currentLine.slope = getLineSlope(heading, position);

    //////////////////
    // Here is the switchpoints between two consecutive goalLines derived.
    // I.e. in which interval a goalLine can be seen as valid.
    // Due to execution time limitations the cutPoints are simply given. The idea is to use
    // trajectorySwitchingPoints(..) that finds out these points in a more complex fasion.
    // PS. trajectorySwitchingPoints(..) needs to be tweaked to work as we want. (was used for
    // something else before)
    //////////////////
    std::vector<int> switchPointsRightGoalLines, switchPointsLeftGoalLines;

    // for (int i = 1; i < rightGoalLines.size() + 1; i++)
    //     {
    // find the intersection point between goalLine i-1 and i
    // switchPoints.push_back(trajectorySwitchingPoints(goalLines[i - 1], goalLines[i]));

    // }
    // Easy fix:
    switchPointsRightGoalLines = cutPoints;
    switchPointsLeftGoalLines = cutPoints;

    //////////////////
    // From the switch points, we can use curve fitting to derive a curve that
    // goes through the switch points. But that is not implemented.
    //////////////////

    //////////////////
    // Create a object that the laneDetector can send to driver.
    //////////////////
    dataToDriver = new LaneDetectorDataToDriver(switchPointsLeftGoalLines, switchPointsRightGoalLines, leftGoalLines, rightGoalLines, currentLine, false, confidenceLevel_goalLine0);

    //////////////////
    // Debug data is provided that is used by lanedetector and lanedetector-inspection modules.
    //////////////////
    if (m_debug)
        {
            finalOutput.cutPoints = cutPoints;
            finalOutput.left = leftSplitted;
            finalOutput.dash = dashToUse;
            finalOutput.right = rightSplitted;
            finalOutput.estimatedLeft =  estimatedLeft;
            finalOutput.estimatedDash =  estimatedDash;
            finalOutput.estimatedRight =  estimatedRight;
            finalOutput.switchPointsLeftGoalLines = switchPointsLeftGoalLines;
            finalOutput.switchPointsRightGoalLines = switchPointsRightGoalLines;
            finalOutput.leftGoalLines = leftGoalLines;
            finalOutput.rightGoalLines = rightGoalLines;
            finalOutput.currentLine = currentLine;
            finalOutput.noTrajectory = false;
        }

    if (printouts)
        {
            cout << "__end createTrajectory" << endl;
        }
}

// Use victors idea and do not transform to bird eye
// std::vector<Point> LineDetector::convertToBirdsEyeView(std::vector<Point> ps)
// {
//     //Convert the point to bird eye view
//     Mat m = getPerspectiveTransform(rect, dst);
//     warped = warpPerspective(ps, M, (maxWidth, maxHeight))

//              return p;
// }

// INFO
// This function is the "new_new" estimationLines used when deriving a trajectory.
// Only provides data for right lane goal line calculations.


void LineDetector::provideGoalLine(EstimationData *ed, GoalLineData *gld)
{
    bool printouts = false;

    int calcRoadAngle = 0, pureRoadSize;
    ed->calcRoadSize = 0;
    ed->isLeftEstimated = false;
    ed->isDashEstimated = false;
    ed->isRightEstimated = false;
    ed->foundGoal = false;
    gld->confidenceLevel_rightGoalLine = 0;
    float roadSizeAdjustment = 1 - (300 - (float(ed->yPosition))) / 350;

    CustomLine lineUsedForEstimation = getNoneCustomLine(); // used for debug of getRoadSize and getRoadAngle
    int whichLine = -1;

    bool foundL = !isNoneCustomLine(ed->left);
    bool foundD = !isNoneCustomLine(ed->dash);
    bool foundR = !isNoneCustomLine(ed->right);

    if (printouts)
        {
            cout << "__start provideGoalLine" << endl;
            cout << "ed->yPosition: " << ed->yPosition << endl;
            cout << "roadSizeAdjustment: " << roadSizeAdjustment << endl;
            cout << "foundL: " << foundL << " foundD: " << foundD << " foundR: " << foundR << endl;
        }


    //yPosition used to get the right roadwidth

    // If we got two lines, we do not need to estimate any lines
    if (foundD)
        {
            lineUsedForEstimation = ed->dash; // used for debug of getRoadSize and getRoadAngle
            whichLine = 1; // used for debug of getRoadSize and getRoadAngle
            calcRoadAngle = getRoadAngle(2, ed->dash.slope);
            ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
            if (printouts)
                cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;

            if (foundL && foundR)
                {
                    // Calculate both goal lines
                    gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
                    gld->confidenceLevel_rightGoalLine = 5;
                    gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->dash, ed);
                }
            else if (foundR)
                {
                    // Calculate right goal line
                    gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
                    gld->confidenceLevel_rightGoalLine = 5;
                    // Shift calculation result to get left goal line
                    gld->leftGoalLine = gld->rightGoalLine;
                    gld->leftGoalLine.p2.x -= ed->calcRoadSize;
                    gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);
                }
            else if (foundL)
                {
                    // Calculate left goal line
                    gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->dash, ed);
                    // Shift calculation result to get right goal line
                    gld->rightGoalLine = gld->leftGoalLine;
                    gld->rightGoalLine.p2.x += ed->calcRoadSize;
                    gld->rightGoalLine.slope = getLineSlope(gld->rightGoalLine.p2, gld->rightGoalLine.p1);
                    gld->confidenceLevel_rightGoalLine = 4;
                }
            else
                {
                    // Only dash found
                    // Estimate either left line and/or right line.
                    // Here I choose to estimate right line
                    if (printouts)
                        cout << "Found only dash, estimating right" << endl;
                    int expectedRightLineX = getIntersectionWithY(ed->dash, ed->yPosition) + ed->calcRoadSize;
                    float expectedRightLineAngle = 180 - abs(ed->dash.slope)
                                                   - calcRoadAngle;
                    if (printouts)
                        {
                            cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
                            cout << "calcRoadAngle: " << calcRoadAngle << endl;
                            cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
                            cout << "abs(ed->dash.slope): " << abs(ed->dash.slope) << endl;
                        }

                    if (expectedRightLineAngle > 90)
                        {
                            expectedRightLineAngle = expectedRightLineAngle - 180;
                        }
                    if (printouts)
                        cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
                    ed->right.slope = expectedRightLineAngle;
                    ed->right.p1.x = expectedRightLineX;
                    ed->right.p1.y = ed->yPosition;
                    ed->isRightEstimated = true;

                    // Calculate right goal line
                    gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
                    gld->confidenceLevel_rightGoalLine = 3;
                    // Shift calculation result to get left goal line
                    gld->leftGoalLine = gld->rightGoalLine;
                    gld->leftGoalLine.p2.x -= ed->calcRoadSize;
                    gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);
                }
        }
    else if (foundL && foundR)
        {
            if (printouts)
                cout << "Found right and left" << endl;
            ed->calcRoadSize = getIntersectionWithY(ed->right, ed->yPosition) - getIntersectionWithY(ed->left, ed->yPosition);
            if (printouts)
                cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
            gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->right, ed);
            // Shift calculation result to get right goal line
            // Shift calculation result to get left goal line
            gld->rightGoalLine = gld->leftGoalLine;
            gld->leftGoalLine.p2.x -= ed->calcRoadSize / 4;
            gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);
            gld->rightGoalLine.p2.x += ed->calcRoadSize / 4;
            gld->rightGoalLine.slope = getLineSlope(gld->rightGoalLine.p2, gld->rightGoalLine.p1);
            gld->confidenceLevel_rightGoalLine = 4;

        }
    else if (foundR)
        {
            lineUsedForEstimation = ed->right; // used for debug of getRoadSize and getRoadAngle
            whichLine = 0; // used for debug of getRoadSize and getRoadAngle
            if (printouts)
                cout << "Found only right, estimating dash" << endl;
            // Estimate dash:
            calcRoadAngle = getRoadAngle(1, ed->right.slope);
            ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;// * (ed->right.slope/50);
            int expectedDashLineX = getIntersectionWithY(ed->right, ed->yPosition) - ed->calcRoadSize;

            float expectedDashLineAngle =  abs(ed->right.slope)
                                           + calcRoadAngle;

            if (printouts)
                {
                    cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
                    cout << "calcRoadAngle: " << calcRoadAngle << endl;
                    cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
                    cout << "expectedDashLineX: " << expectedDashLineX << endl;
                    cout << "abs(ed->right.slope): " << abs(ed->right.slope) << endl;
                }
            if (expectedDashLineAngle > 90)
                {
                    expectedDashLineAngle = expectedDashLineAngle - 180;
                }
            if (printouts)
                cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
            ed->dash.slope = expectedDashLineAngle;
            ed->dash.p1.x = expectedDashLineX;
            ed->dash.p1.y = ed->yPosition;
            ed->isDashEstimated = true;
            // Calculate right goal line
            gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
            gld->confidenceLevel_rightGoalLine = 3;
            // Shift calculation result to get left goal line
            gld->leftGoalLine = gld->rightGoalLine;
            gld->leftGoalLine.p2.x -= ed->calcRoadSize;
            gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);

        }
    else if (foundL)
        {
            lineUsedForEstimation = ed->left; // used for debug of getRoadSize and getRoadAngle
            whichLine = 2; // used for debug of getRoadSize and getRoadAngle
            // Estimate dash:
            if (printouts)
                cout << "Found only left, estimating dash" << endl;
            calcRoadAngle = getRoadAngle(3, ed->left.slope);
            ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
            int expectedDashLineX = getIntersectionWithY(ed->left, ed->yPosition) + ed->calcRoadSize;
            float expectedDashLineAngle =  abs(ed->left.slope)
                                           + calcRoadAngle;

            if (printouts)
                {
                    cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
                    cout << "calcRoadAngle: " << calcRoadAngle << endl;
                    cout << "expectedDashLineX: " << expectedDashLineX << endl;
                    cout << "abs(ed->left.slope): " << abs(ed->left.slope) << endl;
                }
            if (expectedDashLineAngle > 90)
                {
                    expectedDashLineAngle = expectedDashLineAngle - 180;
                }
            if (printouts)
                cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
            ed->dash.slope = expectedDashLineAngle;
            ed->dash.p1.x = expectedDashLineX;
            ed->dash.p1.y = ed->yPosition;
            ed->isDashEstimated = true;
            // Calculation left goal line
            gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->dash, ed);
            // Shift calculation result to get right goal line
            gld->rightGoalLine = gld->leftGoalLine;
            gld->rightGoalLine.p2.x += ed->calcRoadSize;
            gld->rightGoalLine.slope = getLineSlope(gld->rightGoalLine.p2, gld->rightGoalLine.p1);
            gld->confidenceLevel_rightGoalLine = 2;
        }

    // Set the global variable used in charasteristicFiltering
    pureRoadSize = getRoadSize(calcRoadAngle);
    calcRoadSize = pureRoadSize;

    rrd.nmbOfRounds ++; // used for debug of getRoadSize and getRoadAngle
    rrd.whichLine.push_back(whichLine);
    rrd.roadAngle.push_back(calcRoadAngle); // used for debug of getRoadSize and getRoadAngle
    rrd.lineUsed.push_back(lineUsedForEstimation); // used for debug of getRoadSize and getRoadAngle
    rrd.roadSize.push_back(ed->calcRoadSize); // used for debug of getRoadSize and getRoadAngle
    rrd.yPosition.push_back(ed->yPosition); // used for debug of getRoadSize and getRoadAngle

    if (printouts)
        {
            cout << " pureRoadSize: " << pureRoadSize << endl;
            cout << "__end provideGoalLine" << endl;
        }

}

// INFO
// This function is the "new" estimationLines used when deriving a trajectory.
// Only provides data for right lane goal line calculations.

void LineDetector::new_estimateLines(EstimationData *ed)
{
    bool printouts = false;

    if (printouts)
        cout << "\t\t__start new_estimateLines" << endl;
    int calcRoadAngle;
    ed->isLeftEstimated = false;
    ed->isDashEstimated = false;
    ed->isRightEstimated = false;
    ed->foundGoal = false;
    float roadSizeAdjustment = 1 - (300 - (float(ed->yPosition))) / 350;

    bool foundL = !isNoneCustomLine(ed->left);
    bool foundD = !isNoneCustomLine(ed->dash);
    bool foundR = !isNoneCustomLine(ed->right);

    if (printouts)
        {
            cout << "ed->yPosition: " << ed->yPosition << endl;
            cout << "roadSizeAdjustment: " << roadSizeAdjustment << endl;
        }

    cout << "foundL: " << foundL << " foundD: " << foundD << " foundR: " << foundR << endl;

    //yPosition used to get the right roadwidth

    if (foundD)
        {

            if (foundL && foundR)
                {
                    // No estimations needed
                    // Provide data to calculateGoalLine(..)
                    calcRoadAngle = getRoadAngle(2, ed->dash.slope);
                    ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;

                    cout << "Found Left and right and dash" << endl;

                }
            else
                {
                    // TODO:
                    // It may be a good idea to make use of the found left resp. right line when estimating left/right
                    // if (!foundL)
                    //     {
                    //         // Estimate left line
                    //         //offset with half the size of road to the right
                    //         calcRoadAngle = getRoadAngle(2, ed->dash.slope);
                    //         ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
                    //         int expectedLeftLineX = getIntersectionWithY(ed->dash, ed->yPosition) - ed->calcRoadSize;
                    //         float expectedLeftLineAngle = 180 - abs(ed->dash.slope)
                    //                                       - calcRoadAngle;
                    //         if (expectedLeftLineAngle > 90)
                    //             {
                    //                 expectedLeftLineAngle = expectedLeftLineAngle - 180;
                    //             }
                    //         ed->left.slope = expectedLeftLineAngle;
                    //         ed->left.p1.x = expectedLeftLineX;
                    //         ed->left.p1.y = h;
                    //         ed->isLeftEstimated = true;
                    //         cout << "Found dash, estimating left" << endl;
                    //     }
                    if (!foundR)
                        {
                            // Estimate right line
                            calcRoadAngle = getRoadAngle(2, ed->dash.slope);
                            ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
                            int expectedRightLineX = getIntersectionWithY(ed->dash, ed->yPosition) + ed->calcRoadSize;
                            float expectedRightLineAngle = 180 - abs(ed->dash.slope)
                                                           - calcRoadAngle;
                            if (expectedRightLineAngle > 90)
                                {
                                    expectedRightLineAngle = expectedRightLineAngle - 180;
                                }
                            ed->right.slope = expectedRightLineAngle;
                            ed->right.p1.x = expectedRightLineX;
                            ed->right.p1.y = h;
                            ed->isRightEstimated = true;
                            cout << "Found dash, estimating right" << endl;
                        }
                }
            ed->foundGoal = true;
        }
    else
        {
            if (foundL)
                {
                    // Estimate dash line and use left line instead of right line in calculateGoalLine
                    //offset with one and a half the size of road to the right
                    calcRoadAngle = getRoadAngle(3, ed->left.slope);
                    ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
                    int expectedDashLineX = getIntersectionWithY(ed->left, ed->yPosition) + ed->calcRoadSize;
                    float expectedDashLineAngle =  abs(ed->left.slope)
                                                   + calcRoadAngle;
                    if (printouts)
                        {
                            cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
                            cout << "calcRoadAngle: " << calcRoadAngle << endl;
                            cout << "expectedDashLineX: " << expectedDashLineX << endl;
                            cout << "abs(ed->left.slope): " << abs(ed->left.slope) << endl;
                        }
                    if (expectedDashLineAngle > 90)
                        {
                            expectedDashLineAngle = expectedDashLineAngle - 180;
                        }

                    if (m_debug)
                        cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
                    ed->dash.slope = expectedDashLineAngle;
                    ed->dash.p1.x = expectedDashLineX;
                    ed->dash.p1.y = h;
                    ed->isDashEstimated = true;

                    cout << "Found only left line" << endl;
                    ed->foundGoal = true;
                }
            else if (foundR)
                {
                    // Estimate dash line
                    //offset with half the size of road to the left
                    calcRoadAngle = getRoadAngle(1, ed->right.slope);
                    ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
                    int expectedDashLineX = getIntersectionWithY(ed->right, ed->yPosition) - ed->calcRoadSize;

                    float expectedDashLineAngle =  abs(ed->right.slope)
                                                   + calcRoadAngle;
                    if (printouts)
                        {
                            cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
                            cout << "calcRoadAngle: " << calcRoadAngle << endl;
                            cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
                            cout << "expectedDashLineX: " << expectedDashLineX << endl;
                            cout << "abs(ed->right.slope): " << abs(ed->right.slope) << endl;
                        }
                    if (expectedDashLineAngle > 90)
                        {
                            expectedDashLineAngle = expectedDashLineAngle - 180;
                        }
                    if (m_debug)
                        cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;

                    ed->dash.slope = expectedDashLineAngle;
                    ed->dash.p1.x = expectedDashLineX;
                    ed->dash.p1.y = ed->yPosition;
                    ed->isDashEstimated = true;
                    cout << "found only right line" << endl;
                    ed->foundGoal = true;

                }
        }
    if (printouts)
        cout << "\t\t__end new_estimateLines" << endl;
}


// INFO
// This function is the "new_new" calculateGoalLine used when deriving a trajectory
// I want this very generic. given two lines it calculates a goalLine

CustomLine LineDetector::simple_calculateGoalLine(CustomLine fst, CustomLine snd, EstimationData *ed)
{
    bool printouts = false;
    if (printouts)
        cout << "__start simple_calculateGoalLine" << endl;

    CustomLine goalLine;
    Point vp;
    Point goalP;

    //Set goal height
    goalP.y = ed->yPosition;
    int currRoadSize = ed->calcRoadSize; // STRANGE, why is this used?

    // Get the line equation for fst line
    float da = tan(fst.slope * M_PI / 180);
    float db = fst.p1.y - fst.p1.x * da;
    int fstGoalX = (goalP.y - db) / da; // Local var needed, the line may be estimated

    // Get the line equation for the snd line
    float a = tan(snd.slope * M_PI / 180);
    float b = snd.p1.y - snd.p1.x * a;
    int sndGoalX = (goalP.y - b) / a; // Local var needed, the line may be estimated

    //Calculate vanishing point
    //if (da != a) { To avoid float-equal warning
    if (fabs(da - a) > 0.001)
        {
            vp.x = (b - db) / (da - a);
        }
    else
        {
            // Use some default value???
        }
    vp.y = da * vp.x + db;


    int roadSz = (sndGoalX - fstGoalX);
    if (ed->isDashEstimated || ed->isRightEstimated || ed->isLeftEstimated)
        {
            goalP.x = fstGoalX + ed->calcRoadSize * ROAD_GOAL;
        }
    else
        {
            goalP.x = fstGoalX + roadSz * ROAD_GOAL;//(fstGoalX + otherGoalX)/2;//fstGoalX + ROAD_SIZE/2;
        }

    if (printouts)
        {
            cout << "vp (" << vp.x << "," << vp.y << ")" << endl;
            cout << "goalP (" << goalP.x << "," << goalP.y << ")" << endl;
        }
    // ----- Debug stuff follows

    int fstCenterX = (fst.p1.x + fst.p2.x) / 2; // Only for debug
    int fstCenterY = (fst.p1.y + fst.p2.y) / 2; // Only for debug
    if (m_debug)
        {
            // cout << "fst center: " << fstCenterX << "," << fstCenterY << endl;
            // cout << da << "*dx + " << db << endl;
            // cout << "fst line X: " << fstGoalX << endl;
        }

    //mylog << ltu.fstLine.slope << "," << fstCenterX << "," << fstCenterY << "," << roadSz << "," << (180 - abs(ltu.fstLine.slope) - abs(ltu.rightLine.slope)) << endl;
    if (printouts)
        {
            cout << "Road size: " << roadSz << endl;
            cout << "Road angle prediction: " << calcRoadAngle << endl;
            cout << "Road size prediction: " << ed->calcRoadSize << endl;
            cout << "Road angle: "
                 << (180 - abs(fst.slope)
                     - abs(ed->right.slope)) << endl;
            cout << "snd line X: " << sndGoalX << endl;
        }
    //-----------------------
    // Set up goal line
    //-----------------------

    //If we have a goal set the position
    if (printouts)
        {
            cout << "Road size diff: " << abs(currRoadSize - ed->calcRoadSize) << endl;
        }
    if (abs(currRoadSize - ed->calcRoadSize) < 0.5 * currRoadSize) // Strange, this will always be true
        {
            //Set your goal
            goalLine.p1 = vp;
            goalLine.p2 = goalP;
            goalLine.slope = getLineSlope(vp, goalP);
        }
    else
        {
            cout << "Road size diff to high, no goalLine will be provided " << endl;
            goalLine = getNoneCustomLine();
        }
    if (printouts)
        cout << "__end simple_calculateGoalLine" << endl;

    return goalLine;
}

CustomLine LineDetector::new_calculateGoalLine(EstimationData *ed)
{
    bool printouts = false;
    if (printouts)
        cout << "__start new_calculateGoalLine" << endl;

    ltu.lines = new Lines(ltu.leftLineVec, ltu.dashLineVec, ltu.rightLineVec);

    CustomLine goalLine, other;
    Point vp;
    Point goalP;
    if (printouts)
        cout << "ed->foundGoal " << ed->foundGoal << endl;

    if (ed->foundGoal == false)
        {
            return goalLine;
        }

    if (!isNoneCustomLine(ed->right))
        other = ed->right;
    else
        other = ed->left;

    other = ed->right;
    // If any line is estimated, goalP.x is calculated differently
    bool linesEstimated = false;
    if (ed->isDashEstimated || ed->isRightEstimated)
        {
            linesEstimated = true;
        }

    //Set goal height
    goalP.y = ed->yPosition;
    int currRoadSize = ed->calcRoadSize; // STRANGE, why is this used?

    // Get the line equation for dashed line
    float da = tan(ed->dash.slope * M_PI / 180);
    float db = ed->dash.p1.y - ed->dash.p1.x * da;
    int dashGoalX = (goalP.y - db) / da; // Local var needed, the line may be estimated

    // Get the line equation for the other line
    float a = tan(other.slope * M_PI / 180);
    float b = other.p1.y - other.p1.x * a;
    int otherGoalX = (goalP.y - b) / a; // Local var needed, the line may be estimated

    //Calculate vanishing point
    //if (da != a) { To avoid float-equal warning
    if (fabs(da - a) > 0.001)
        {
            vp.x = (b - db) / (da - a);
        }
    else
        {
            // Use some default value???
        }
    vp.y = da * vp.x + db;

    cout << "vp (" << vp.x << "," << vp.y << ")" << endl;

    int roadSz = (otherGoalX - dashGoalX);
    if (linesEstimated)
        {
            goalP.x = dashGoalX + ed->calcRoadSize * ROAD_GOAL;
        }
    else
        {
            goalP.x = dashGoalX + roadSz * ROAD_GOAL;//(dashGoalX + otherGoalX)/2;//dashGoalX + ROAD_SIZE/2;
        }

    cout << "goalP (" << goalP.x << "," << goalP.y << ")" << endl;
    // ----- Debug stuff follows

    int dashCenterX = (ed->dash.p1.x + ed->dash.p2.x) / 2; // Only for debug
    int dashCenterY = (ed->dash.p1.y + ed->dash.p2.y) / 2; // Only for debug
    if (m_debug)
        {
            cout << "Dash center: " << dashCenterX << "," << dashCenterY << endl;
            //cout << da << "*dx + " << db << endl;
            cout << "Dash line X: " << dashGoalX << endl;
        }

    //mylog << ltu.dashLine.slope << "," << dashCenterX << "," << dashCenterY << "," << roadSz << "," << (180 - abs(ltu.dashLine.slope) - abs(ltu.rightLine.slope)) << endl;
    if (m_debug)
        {
            cout << "Road size: " << roadSz << endl;
            cout << "Road angle prediction: " << calcRoadAngle << endl;
            cout << "Road size prediction: " << ed->calcRoadSize << endl;
            cout << "Road angle: "
                 << (180 - abs(ed->dash.slope)
                     - abs(ed->right.slope)) << endl;
            cout << "Right line X: " << otherGoalX << endl;
        }
    //-----------------------
    // Set up goal line
    //-----------------------

    //If we have a goal set the position
    if (m_debug)
        {
            cout << "Road size diff: " << abs(currRoadSize - ed->calcRoadSize) << endl;
        }
    if (abs(currRoadSize - ed->calcRoadSize) < 0.5 * currRoadSize) // Strange, this will always be true
        {
            //Set your goal
            goalLine.p1 = vp;
            goalLine.p2 = goalP;
            goalLine.slope = getLineSlope(vp, goalP);
            ltu.lines->setGoalLine(goalLine);

            //Assume this position as the car position
            Point position;
            position.x = w / 2;
            position.y = h;
            Point heading;
            heading.x = w / 2;
            heading.y = 0;
            //Create car orientation vector
            CustomLine current;
            current.p1 = heading;
            current.p2 = position;
            current.slope = getLineSlope(heading, position);
            ltu.lines->setCurrentLine(current);
        }
    else
        {
            cout << "CASE: NONE " << endl;
        }
    cout << "__end new_calculateGoalLine" << endl;
    //mylog.close();
    return goalLine;
}

// This function is tested and working
std::vector<Point> LineDetector::trajectorySwitchingPoints(std::vector<CustomLine> lines)
{
    bool printouts = false;
    if (printouts)
        cout << "__getTrajectoryPoints START" << endl;
    std::vector<Point> points;

    // If empty vector, return
    if (lines.size() == 0)
        {
            if (printouts)
                cout << "__getTrajectoryPoints END" << endl;
            return points;

        }
    // If one line in vector
    else if (lines.size() == 1)
        {
            // Get line eq.
            float a = tan(lines[0].slope * M_PI / 180);
            float b = lines[0].p1.y - lines[0].p1.x * a;

            Point down;
            down.y = h;
            down.x = (h - b) / a;
            points.push_back(down);

            // Find point where the line intersects with the top of the screen
            Point up;
            up.y = 0;
            up.x = (0 - b) / a;
            points.push_back(up);

        }
    // If more than one line in vector
    else
        {
            for (int i = 0; i < lines.size() + 1; i++)
                {
                    Point p;

                    // Find point where the line intersects with bottom of the frame
                    if (i == 0)
                        {
                            // Get line eq.
                            float a = tan(lines[i].slope * M_PI / 180);
                            float b = lines[i].p1.y - lines[i].p1.x * a;

                            p.y = h;
                            p.x = (h - b) / a;
                            points.push_back(p);
                            if (printouts)
                                cout << "fst point: " << p << endl;

                            // Find point where the line intersects with the top of the screen
                        }
                    else if (i == lines.size())
                        {
                            // Get line eq.
                            float a = tan(lines[i - 1].slope * M_PI / 180);
                            float b = lines[i - 1].p1.y - lines[i - 1].p1.x * a;

                            p.y = 0;
                            p.x = (0 - b) / a;
                            points.push_back(p);
                            if (printouts)
                                cout << "last point: " << p << endl;

                            // Find point where the lines intersect
                        }
                    else
                        {
                            if (printouts)
                                {
                                    cout << "line1 " <<  lines[i - 1].p1 << "," << lines[i - 1].p2 << ")" << endl;
                                    cout << "line2 " <<  lines[i].p1 << "," << lines[i].p2 << ")" << endl;
                                }

                            // Get the line equation for first line
                            float da = tan(lines[i - 1].slope * M_PI / 180);
                            float db = lines[i - 1].p1.y - lines[i - 1].p1.x * da;

                            // Get the line equation for second line
                            float a = tan(lines[i].slope * M_PI / 180);
                            float b = lines[i].p1.y - lines[i].p1.x * a;

                            //Calculate intersection point
                            if (fabs(da - a) > 0.001)
                                {
                                    p.x = (b - db) / (da - a);
                                }
                            else
                                {
                                    // Use some default value???
                                }
                            p.y = da * p.x + db;

                            // If lines do not intersect cleanly, use the goalLine start as switching point
                            if (p.y < lines[i].p2.y)
                                {
                                    p.y = lines[i].p1.y;
                                }
                            points.push_back(p);
                            cout << "point " << i << ": " << p << endl;
                        }
                    if (printouts)
                        {
                            cout << "Points: " << endl;
                            for (int i = 0; i < points.size(); i++)
                                cout << points[i] ;
                            cout << endl;
                        }
                }
        }
    if (printouts)
        cout << "__getTrajectoryPoints END" << endl;
    return points;
}

//////////////////
// A wrapper for splitContourAtPoints
//////////////////
std::vector<CustomLine> LineDetector::splitSolidLines(std::vector<int> cutAt, CustomLine solid)
{
    bool printouts = false; // Gives debug text and window
    bool deactivateSplitting = false;
    cv::Mat out;

    if (printouts)
        {
            cout << "__start splitSolidLines" << endl;
            out = m_frame.clone();
        }
    std::vector<CustomLine> splittedSolid;

    //////////////////
    // Check whether it is necessary to split the solid line e.g. it is a straight line
    // Test shows that it is best to always split.
    //////////////////
    if (line_sizes[solid.polygonIndex].sizeX < 30 || deactivateSplitting)  // pixel width of the solid line's rectangle
        {
            // Return vector with replicated solid lines
            std::vector<CustomLine> unCutSolid (cutAt.size() + 1, solid);
            return unCutSolid;
        }

    //////////////////
    // Transform the cut point int vector into a Point vector
    //////////////////
    std::vector<Point> cutPoints;
    for (int i = 0; i < cutAt.size(); i++)
        {
            Point p;
            p.y = cutAt[i];
            p.x = 0;
            cutPoints.push_back(p);

            if (printouts)
                {
                    Point q = p;
                    q.x = 800;
                    line(out, p, q, Scalar(255, 0, 0));
                }
        }

    //////////////////
    // Cut the provided solid line
    //////////////////
    std::vector<RotatedRect> rectangles = splitContourAtPoints(cutPoints, solid.polygonIndex, true);

    //////////////////
    // Transform the vector of RotatedRect into Customlines
    //////////////////
    for (int i = 0; i < rectangles.size(); i++)
        {
            // Check if it was possible to make a certain cut
            Point2f none;
            Point2f rect_points[4];
            rectangles[i].points(rect_points);

            if (printouts)
                {
                    for (int j = 0; j < 4; j++)
                        {
                            line(out, rect_points[j], rect_points[(j + 1) % 4], Scalar(255, 0, 0));
                        }
                }
            // Check if a valid RotatedRect was provided
            if (rect_points[0] == none && rect_points[1] == none)
                {
                    if (printouts)
                        cout << "None rect. point[0]: " << rect_points[0] << endl;
                    splittedSolid.push_back(getNoneCustomLine());
                }
            else
                // if cut is valid, create line
                {
                    PolySize attr = createPolySize(rectangles[i]);
                    Point rectCenter;
                    rectCenter.x = rectangles[i].center.x;
                    rectCenter.y = rectangles[i].center.y;
                    rectangles[i].angle = getLineSlope(attr.shortSideMiddle, rectCenter);
                    splittedSolid.push_back(createLineFromRect(&rectangles[i], attr.sizeX, attr.sizeY, -1));
                }
            if (printouts)
                {
                    cout << "splittedSolid[" << i << "] slope: " << splittedSolid[i].slope << " x: " << splittedSolid[i].p1.x << " y: " << splittedSolid[i].p1.y << endl;
                    cout << "splittedSolid[" << i << "] slope: " << splittedSolid[i].slope << " x: " << splittedSolid[i].p2.x << " y: " << splittedSolid[i].p2.y << endl;
                }

        }

    if (printouts)
        {
            imshow("Most recent splitted rect", out);
            cout << "__end splitSolidLines" << endl;
        }
    return splittedSolid;
}
CustomLine LineDetector::getNoneCustomLine()
{
    CustomLine none;
    none.p1.x = 0;
    none.p1.y = 0;
    none.p2.x = 0;
    none.p2.y = 0;
    return none;
}
bool LineDetector::isNoneCustomLine(CustomLine aspirant)
{
    if (aspirant.p1.x == 0 && aspirant.p1.y == 0 && aspirant.p2.x == 0 && aspirant.p2.y == 0)
        return true;
    return false;
}
// The old estimateLines
void LineDetector::estimateLines(LinesToUse *ltu)
{
    ltu->isDashEstimated = false;
    ltu->isRightEstimated = false;
    ltu->foundGoal = false;

    if (ltu->foundD)
        {

            if (ltu->foundL && ltu->foundR)
                {
                    // No estimations needed
                    // Provide data to calculateGoalLine(..)
                    calcRoadAngle = getRoadAngle(2, ltu->dashLine.slope);
                    calcRoadSize = getRoadSize(calcRoadAngle);
                    cout << "Found Left and right and dash:" << endl;

                }
            else
                {
                    if (ltu->foundL)
                        {
                            // Estimate right line
                            //offset with half the size of road to the right
                            calcRoadAngle = getRoadAngle(2, ltu->dashLine.slope);
                            calcRoadSize = getRoadSize(calcRoadAngle);
                            int expectedRightLineX = currentDashGoalX - calcRoadSize;
                            float expectedRightLineAngle = 180 - abs(ltu->dashLine.slope)
                                                           - calcRoadAngle;
                            if (expectedRightLineAngle > 90)
                                {
                                    expectedRightLineAngle = expectedRightLineAngle - 180;
                                }
                            ltu->rightLine.slope = expectedRightLineAngle;
                            ltu->rightLine.p1.x = expectedRightLineX;
                            ltu->rightLine.p1.y = h;
                            ltu->isRightEstimated = true;
                            cout << "Found Left and dash:" << endl;
                        }
                    if (ltu->foundR)
                        {
                            calcRoadAngle = getRoadAngle(2, ltu->dashLine.slope);
                            calcRoadSize = getRoadSize(calcRoadAngle);
                            int expectedLeftLineX = currentDashGoalX + calcRoadSize;
                            float expectedLeftLineAngle = 180 - abs(ltu->dashLine.slope)
                                                          - calcRoadAngle;
                            if (expectedLeftLineAngle > 90)
                                {
                                    expectedLeftLineAngle = expectedLeftLineAngle - 180;
                                }
                            ltu->leftLine.slope = expectedLeftLineAngle;
                            ltu->leftLine.p1.x = expectedLeftLineX;
                            ltu->leftLine.p1.y = h;
                            ltu->isLeftEstimated = true;
                            cout << "Found Right and dash" << endl;
                        }
                }
            ltu->foundGoal = true;


        }
    else
        {
            if (ltu->foundL)
                {
                    // Estimate dash line and use left line instead of right line
                    //offset with one and a half the size of road to the right
                    calcRoadAngle = getRoadAngle(3, ltu->leftLine.slope);
                    calcRoadSize = getRoadSize(calcRoadAngle);
                    int expectedDashLineX = currentLeftGoalX + calcRoadSize;
                    float expectedDashLineAngle =  abs(ltu->leftLine.slope)
                                                   + calcRoadAngle;
                    if (expectedDashLineAngle > 90)
                        {
                            expectedDashLineAngle = 180 - expectedDashLineAngle;
                        }

                    ltu->dashLine.slope = expectedDashLineAngle;
                    ltu->dashLine.p1.x = expectedDashLineX;
                    ltu->dashLine.p1.y = h;
                    ltu->isDashEstimated = true;
                    ltu->foundGoal = true;
                    cout << "Found only left" << endl;
                }
            else if (ltu->foundR)
                {
                    // Estimate dash line
                    //offset with half the size of road to the left
                    calcRoadAngle = getRoadAngle(1, ltu->rightLine.slope);
                    calcRoadSize = getRoadSize(calcRoadAngle);
                    int expectedDashLineX = currentRightGoalX - calcRoadSize;
                    float expectedDashLineAngle =  abs(ltu->rightLine.slope)
                                                   + calcRoadAngle;
                    if (expectedDashLineAngle > 90)
                        {
                            expectedDashLineAngle = 180 - expectedDashLineAngle;
                        }

                    ltu->dashLine.slope = expectedDashLineAngle;
                    ltu->dashLine.p1.x = expectedDashLineX;
                    ltu->dashLine.p1.y = h;
                    ltu->isDashEstimated = true;
                    ltu->foundGoal = true;
                    cout << "found only right" << endl;

                }
        }

    return;
}
// The old calculateGoalLine
void LineDetector::calculateGoalLine(LinesToUse *ltu)
{
    //-----------------------
    // Set up current heading line and goal line
    //-----------------------

    ltu->lines = new Lines(ltu->leftLineVec, ltu->dashLineVec, ltu->rightLineVec);
    // Check whether to run the function
    if (!(ltu->foundGoal))
        {
            if (m_debug)
                {
                    cout << "CASE: NO FOUND GOAL" << endl;
                }
            return;
        }

    // If any line is estimated, goalP.x is calculated differently
    bool linesEstimated = false;
    if (ltu->isDashEstimated || ltu->isLeftEstimated)
        {
            linesEstimated = true;
        }

    // If only left line was present, use that as the right line
    /* if (ltu->isDashEstimated == true && ltu->isLeftEstimated == true)
         {
             ltu->leftLine = ltu->rightLine;
         }*/

    Point vpl, vpr;
    Point goalPl, goalPr;
    //Set goal height
    goalPl.y = h;
    goalPr.y = h;
    int currRoadSize = calcRoadSize;

    // Get the line equation for dashed line
    float da = tan(ltu->dashLine.slope * M_PI / 180);
    float db = ltu->dashLine.p1.y - ltu->dashLine.p1.x * da;
    int dashGoalX = (goalPr.y - db) / da; // Local var needed, the line may be estimated


    // Get the line equation for right line
    float ar = tan(ltu->rightLine.slope * M_PI / 180);
    float br = ltu->rightLine.p1.y - ltu->rightLine.p1.x * ar;
    int rightGoalX = (goalPr.y - br) / ar; // Local var needed, the line may be estimated

    // Get the line equation for left line
    float al = tan(ltu->leftLine.slope * M_PI / 180);
    float bl = ltu->leftLine.p1.y - ltu->leftLine.p1.x * al;
    int leftGoalX = (goalPl.y - bl) / al; // Local var needed, the line may be estimated

    //Calculate vanishing point
    //if (da != a) { To avoid float-equal warning
    if (fabs(da - ar) > 0.001)
        {
            vpr.x = (br - db) / (da - ar);
        }
    else
        {
            // Use some default value???
        }
    vpr.y = da * vpr.x + db;


    if (fabs(da - al) > 0.001)
        {
            vpl.x = (bl - db) / (da - al);
        }
    else
        {
            // Use some default value???
        }
    vpl.y = da * vpl.x + db;

    int roadSzl = (leftGoalX - dashGoalX);
    int roadSzr = (rightGoalX - dashGoalX);
    if (ltu->isDashEstimated || ltu->isRightEstimated)
        {

            goalPr.x = dashGoalX + calcRoadSize * ROAD_GOAL;
        }
    else
        {
            goalPr.x = dashGoalX + roadSzr * ROAD_GOAL;

        }
    if (ltu->isDashEstimated || ltu->isLeftEstimated)
        {
            goalPl.x = dashGoalX - calcRoadSize * ROAD_GOAL;
        }
    else
        {
            goalPl.x = dashGoalX + roadSzl * ROAD_GOAL;//(dashGoalX + rightGoalX)/2;//dashGoalX + ROAD_SIZE/2;
        }

    // ----- Debug stuff follows

    int dashCenterX = (ltu->dashLine.p1.x + ltu->dashLine.p2.x) / 2; // Only for debug
    int dashCenterY = (ltu->dashLine.p1.y + ltu->dashLine.p2.y) / 2; // Only for debug
    if (m_debug)
        {
            cout << "Dash center: " << dashCenterX << "," << dashCenterY
                 << endl;
            cout << "Dash line X: " << dashGoalX << endl;
            cout << "Road size Left: " << roadSzl << endl;
            cout << "Road size Right: " << roadSzr << endl;
            cout << "Road angle prediction: " << calcRoadAngle << endl;
            cout << "Road size prediction: " << calcRoadSize << endl;
            cout << "Left line X: " << leftGoalX << endl;
            cout << "Right line X: " << rightGoalX << endl;

        }


    //If we have a goal set the position and
    if (m_debug)
        {
            cout << "Road size diff: " << abs(currRoadSize - calcRoadSize)
                 << endl;


        }
    if (abs(currRoadSize - calcRoadSize) < 0.5 * currRoadSize)
        {
            //Assume this position as the car position
            Point position;
            position.x = w / 2;
            position.y = h;
            Point heading;
            heading.x = w / 2;
            heading.y = 0;
            //Create car orientation vector
            CustomLine current;
            current.p1 = heading;
            current.p2 = position;
            current.slope = getLineSlope(heading, position);
            ltu->lines->setCurrentLine(current);
            //Set your goal
            CustomLine goal;
            goal.p1 = vpl;
            goal.p2 = goalPl;
            goal.slope = getLineSlope(vpl, goalPl);
            ltu->lines->setGoalLineLeft(goal);


            goal.p1 = vpr;
            goal.p2 = goalPr;
            goal.slope = getLineSlope(vpr, goalPr);
            ltu->lines->setGoalLine(goal);

            cout << "LINES: " << endl;
            //cout << "leftLine: " << ltu->lines->leftLine << endl;
            //cout << "dashedLine: " << ltu->lines->dashedLine << endl;
            //cout << "rightLine: " << ltu->lines->rightLine << endl;

        }
    else
        {
            cout << "CASE: NONE " << endl;
        }
    //mylog.close();
    return;
}
// Helper functions follows

std::vector<CustomLine> LineDetector::findCurve(std::vector<CustomLine> lines)
{
    bool printouts = false;
    if (printouts)
        cout << "__running findCurves" << endl;
    // This function is used to merge the dashes to one curve, or
    // used to be sure that you have found the dashes.
    std::vector<CustomLine> curve;
    if (lines.size() < 2)
        {
            if (printouts)
                cout << "findCurve need at least 2 lines. returning" << endl;
            return curve;
        }
    if (printouts)
        {
            for (int j = 0; j < lines.size(); j++)
                {
                    cout << "Dash line. p1(" << lines[j].p1.x << "," << lines[j].p1.y << ") p2(" << lines[j].p2.x << "," << lines[j].p2.y << ")" << endl;
                }
        }
    for (int j = 1; j < lines.size(); j++)
        {
            // The snd dash has to be above the fst one
            if (lines[0].p2.y > lines[j].p1.y)
                {
                    // The slope diviation between the dashes is limited
                    float slopeA, slopeB;
                    if (lines[0].slope < 0)
                        slopeA = lines[0].slope + 180;
                    else
                        slopeA = lines[0].slope;

                    if (lines[j].slope < 0)
                        slopeB = lines[j].slope + 180;
                    else
                        slopeB = lines[j].slope;

                    // The slope of a line drawn between the real lines
                    float slopeInBetween = getLineSlope(lines[0].p2, lines[j].p1);

                    if (slopeInBetween < 0)
                        slopeInBetween += 180;

                    // The slope deviation between the lines
                    float slopeDiffLines = abs(slopeA - slopeB);
                    float slopeDiffToLine0 = abs(slopeInBetween - slopeA);
                    float slopeDiffToLineJ = abs(slopeInBetween - slopeB);
                    float distInBetween = getDist(lines[0].p2, lines[j].p1);

                    if (printouts)
                        {
                            cout << "slopeA: " << slopeA;
                            cout << " slopeB: " << slopeB;
                            cout << " slopeInBetween: " << slopeInBetween << endl;
                            cout << " distInBetween: " << distInBetween << endl;
                            cout << " slopeDiffLines: " << slopeDiffLines;
                            cout << " slopeDiffToLine0: " << slopeDiffToLine0;
                            cout << " slopeDiffToLineJ: " << slopeDiffToLineJ << endl;
                        }

                    if ((slopeDiffLines < 60) && (slopeDiffToLine0 < slopeDiffLines + 20) &&
                            (slopeDiffToLineJ < slopeDiffLines + 20) && (distInBetween < 150))
                        {
                            if (printouts)
                                {
                                    cout << "curve found";
                                    cout << "(" << lines[j].p1.x << "," << lines[j].p1.y << ") ";
                                    cout << "(" << lines[j].p2.x << "," << lines[j].p2.y << ") " << endl;
                                }

                            curve.push_back(lines[j]);

                            if (lines.size() > 2)
                                {
                                    // Call recursively, but take away the already processed line
                                    lines[0] = lines[j];
                                    lines.erase(lines.begin() + j);
                                    std::vector<CustomLine> res = findCurve(lines);

                                    // Add the recursive result
                                    for (int j = 0; j < res.size(); j++)
                                        {
                                            curve.push_back(res[j]);
                                        }
                                    return curve;
                                }
                            else
                                {
                                    if (printouts)
                                        cout << "__longest curve found. returning" << endl;
                                    return curve;
                                }
                        }
                }
        }
    if (printouts)
        cout << "__No curve found, returning." << endl;
    return curve;
}

CustomLine LineDetector::createLineFromRect(RotatedRect *rect, int sizeX, int sizeY, int polygonIndex)
{
    bool printouts = false;
    if (printouts)
        cout << "__start createLineFromRect" << endl;

    Point2f rect_points[4];
    rect->points (rect_points);

    if (printouts)
        {
            cout << "Sizes: " << sizeX << " " << sizeY << endl;;
            for (int j = 0; j < 4; j++)
                cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y << "]" << endl;
        }

    CustomLine l;
    Point pt1, pt2;
    l.polygonIndex = polygonIndex;
    //cout << "[centerx, centery] = [" << rect->center.x << "," << rect->center.y << "]" << endl;
    if (rect->angle < 90)
        {
            float angle = rect->angle * M_PI / 180;
            float xOffset = cos(angle) * sizeY / 2;
            float yOffset = sin(angle) * sizeY / 2;
            pt1.y = rect->center.y + yOffset;
            pt1.x = rect->center.x + xOffset;
            pt2.y = rect->center.y - yOffset;
            pt2.x = rect->center.x - xOffset;
        }
    else
        {
            rect->angle = rect->angle - 180;
            float angle = (-rect->angle) * M_PI / 180;
            float xOffset = cos(angle) * sizeY / 2;
            float yOffset = sin(angle) * sizeY / 2;
            pt1.y = rect->center.y + yOffset;
            pt1.x = rect->center.x - xOffset;
            pt2.y = rect->center.y - yOffset;
            pt2.x = rect->center.x + xOffset;
        }
    //cout << "Angle: " << rect->angle << endl;
    //cout << "[x, y] = [" << pt1.x << "," << pt1.y << "]" << endl;
    l.p1 = pt1;
    l.p2 = pt2;
    l.slope = rect->angle;
    if (printouts)
        cout << "__end createLineFromRect" << endl;
    return l;
}

int LineDetector::detectHorizontalLine(Mat canny_roi, int dist)
{
    vector<Vec4i> lines;
    // Hough line detection
    HoughLinesP(canny_roi, lines, 1, CV_PI / 180, 50, 100, 100);
    vector<Vec4i> likely_lines;
    for (vector<Vec4i>::iterator it = lines.begin(); it != lines.end(); it++)
        {
            int xA = (*it)[0], yA = (*it)[1];
            int xB = (*it)[2], yB = (*it)[3];
            double theta = atan2(yB - yA, xB - xA);
            //cout << "Angle: " << theta*180/CV_PI << endl;
            if (theta >= -CV_PI / 36 && theta <= CV_PI / 36)
                {
                    //&&_roi.cols/2 && xB >= src_roi.cols/2) {
                    likely_lines.push_back(*it);
                    //cout << "(" << xA << ", " << yA << "), (" << xB << ", " << yB << ")" << endl;
                }
        }
    int yMax = 0;
    vector<Vec4i>::iterator ptr1, ptr2;
    for (vector<Vec4i>::iterator it1 = likely_lines.begin();
            it1 != likely_lines.end(); it1++)
        {
            for (vector<Vec4i>::iterator it2 = it1 + 1; it2 != likely_lines.end();
                    it2++)
                {
                    Point p1A = Point((*it1)[0], (*it1)[1]);
                    Point p1B = Point((*it1)[2], (*it1)[3]);
                    Point p2A = Point((*it2)[0], (*it2)[1]);
                    Point p2B = Point((*it2)[2], (*it2)[3]);
                    int y1Avg = (p1A.y + p1B.y) / 2;
                    int y2Avg = (p2A.y + p2B.y) / 2;
                    if (abs(y1Avg - y2Avg) <= dist)
                        {
                            if (max(y1Avg, y2Avg) > yMax)
                                {
                                    yMax = max(y1Avg, y2Avg);
                                    ptr1 = it1;
                                    ptr2 = it2;
                                }
                        }
                }
        }
    if (yMax > 0)
        {
            return canny_roi.rows - yMax;
        }
    else
        {
            return -1;
        }
}

float LineDetector::getDist(const Point p1, const Point p2) const
{
    return sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2));
}

int LineDetector::detectStartLine(int dist)
{
    Rect roi_left, roi_right;
    roi_left = Rect(0, m_frameCanny.rows / 2, m_frameCanny.cols / 2,
                    m_frameCanny.rows / 2);
    roi_right = Rect(m_frameCanny.cols / 2, m_frameCanny.rows / 2,
                     m_frameCanny.cols / 2, m_frameCanny.rows / 2);
    int yLineLeft = detectHorizontalLine(m_frameCanny(roi_left), dist);
    int yLineRight = detectHorizontalLine(m_frameCanny(roi_right), dist);
    if (abs(yLineLeft - yLineRight) <= 10)
        {
            return min(yLineLeft, yLineRight);
        }
    else
        {
            return -1;
        }
}

int LineDetector::detectStopLine(int dist)
{
    Mat src_roi;
    // Cut out the lower right corner
    Rect roi = Rect(m_frameCanny.cols / 2, m_frameCanny.rows / 2,
                    m_frameCanny.cols / 2, m_frameCanny.rows / 2);
    src_roi = m_frameCanny(roi);
    return detectHorizontalLine(src_roi, dist);
}

/** Get the slope of a line defined by two points */
float LineDetector::getLineSlope(Point &p1, Point &p2)
{
    float slope = M_PI / 2;
    if ((p1.x - p2.x) != 0)
        {
            slope = (p1.y - p2.y) / ((float) (p1.x - p2.x));
            slope = atan(slope);
        }
    if (slope < 0)
        {
            return 180 + (slope * 180 / M_PI);
        }
    return slope * 180 / M_PI;
}

/** Predicts the road angle considering one detected line */
int LineDetector::getRoadAngle(int lineDetected, int lineAngle)
{
    int roadAngleNow = ROAD_ANGLE; // Previous declaration was roadAngleNow. Shadows the global variable
    float c1 = (roadAngleNow - 29.1)
               / (180 - abs(MID_DASH_ANGLE) - roadAngleNow);
    float c2 = (roadAngleNow - 65.0) / (MID_DASH_ANGLE + 90);
    //cout << "Road angle consts: " << c1 << "," << c2 << endl;
    switch (lineDetected)
        {
        case 1:
        {
            //founded line is right line
            if (lineAngle < 63 && lineAngle >= 25)
                {
                    roadAngleNow = 29.1 + c1 * lineAngle; //1.44
                }
            else if (lineAngle < 25)
                {
                    roadAngleNow = 65;
                }
        }
        ;
        break;
        case 2:
        {
            //founded line is dash line
            if (lineAngle < 0)
                {
                    roadAngleNow = 65 + (lineAngle + 90) * c2; //0.59;
                }
            else
                {
                    roadAngleNow = 65 + (lineAngle - 90) * c2; //0.59;
                }
        }
        ;
        break;
        case 3:
        {
            //founded line is left line
            if (lineAngle > -63 && lineAngle <= -25)
                {
                    roadAngleNow = 29.1 - c1 * lineAngle; //1.44
                }
            else if (lineAngle > -25)
                {
                    roadAngleNow = 65;
                }
        }
        ;
        break;
        }
    return roadAngleNow;
}

/** Predicts the road size considering the roadAngle */
int LineDetector::getRoadSize(int roadAngleVal)
{
    bool printouts =  false;
    int roadSizeNow = ROAD_SIZE; // Previous declaration was roadSizeNow. Shadows the global variable

    if (roadAngleVal > ROAD_ANGLE && roadAngleVal < (ROAD_ANGLE + 15))
        {
            roadSizeNow = 5 * roadAngleVal + (ROAD_SIZE - ROAD_ANGLE * 5);
        }
    else if (roadAngleVal > (ROAD_ANGLE + 15))
        {
            float a = (ROAD_SIZE - 5) / 5;
            float b = 3 * ROAD_SIZE - (ROAD_ANGLE + 25) * a;
            roadSizeNow = roadAngleVal * a + b;
        }
    else if (roadAngleVal > (ROAD_ANGLE - 15) && roadAngleVal < ROAD_ANGLE)
        {
            roadSizeNow = 5 * (2 * ROAD_ANGLE - roadAngleVal)
                          + (ROAD_SIZE - ROAD_ANGLE * 5);
        }
    else if (roadAngleVal < (ROAD_ANGLE - 15)
             && roadAngleVal > (ROAD_ANGLE - 25))
        {
            //cout << "SZ S" << endl;
            float a = (ROAD_SIZE - 5) / 5;
            float b = 3 * ROAD_SIZE - (ROAD_ANGLE + 25) * a;
            roadSizeNow = (2 * ROAD_ANGLE - roadAngleVal) * a + b;
        }

    return roadSizeNow;
}

Point2f LineDetector::getWorldPoint(Point2i p)
{
    p.x = p.x - 376;
    p.y = 480 - p.y;

    CameraStruct cam;
    cam.focal = 420;
    cam.focal2 = 420;
    cam.u0 = 752 / 2;
    cam.v0 = 240;
    cam.height = 192;
    cam.length = 88;
    cam.alpha = 24;
    cam.beta = 87;
    cam.gamma = 90;
    cam.size.width = w;
    cam.size.height = h;

    /*double camera_matrix[3][3] =
     {-0.688196, -4.10335, 13.8586,
     -0.0523179, -6.23605, 69.5388,
     -0.000121023, -0.022983, -0.559884,
     };*/

    Mat camera_matrix = getBirdTransMatrix(cam);
    /*cout << "Transform matrix" << endl;
     for(int i=0; i < camera_matrix.size().height; i++) {
     for(int j=0; j < camera_matrix.size().width; j++) {
     cout << camera_matrix.at<double>(j,i) << " ";
     }
     cout << endl;
     }*/

    //
    double u, v, wi; // wi was called w before. (To avoid variable shadows)
    Point2f res;
    wi = 1
         / (camera_matrix.at<double>(2, 0) * p.x
            + camera_matrix.at<double>(2, 1) * p.y
            + camera_matrix.at<double>(2, 2));
    u = wi * p.x;
    v = wi * p.y;

    res.x = camera_matrix.at<double>(0, 0) * u
            + camera_matrix.at<double>(0, 1) * v
            + camera_matrix.at<double>(0, 2) * wi;
    res.y = camera_matrix.at<double>(1, 0) * u
            + camera_matrix.at<double>(1, 1) * v
            + camera_matrix.at<double>(1, 2) * wi;

    return res;
}

int LineDetector::getIntersectionWithBottom(CustomLine l) const
{
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if (abs(a) > 0.001)
        {
            positionX = (h - b) / a;
        }
    return positionX;
}

int LineDetector::getIntersectionWithTop(CustomLine l) const
{
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if (abs(a) > 0.001)
        {
            positionX = (0 - b) / a;
        }
    return positionX;
}

int LineDetector::getIntersectionWithY(CustomLine l, int y) const
{
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if (abs(a) > 0.001)
        {
            positionX = (y - b) / a;
        }
    return positionX;
}

IntermediateResult_getContours *LineDetector::getResult_getContours()
{
    return &result_getContours;
}

IntermediateResult_getRectangles *LineDetector::getResult_getRectangles()
{
    return &result_getRectangles;
}

IntermediateResult *LineDetector::getResult_classification()
{
    return &result_classification;
}

IntermediateResult *LineDetector::getResult_filterAndMerge()
{
    return &result_filterAndMerge;
}

IntermediateResult *LineDetector::getResult_finalFilter()
{
    return &result_finalFilter;
}

LinesToUse *LineDetector::getResult_calculateGoalLine()
{
    return &ltu;
}

LaneDetectorDataToDriver *LineDetector::getDriverData()
{
    return dataToDriver;
}

FinalOutput *LineDetector::getResult_createTrajectory()
{
    return &finalOutput;
}

}

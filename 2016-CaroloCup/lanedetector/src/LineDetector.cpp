#include "LineDetector.h"
#include "Transforms.h"
#include <fstream>
#include <queue>
#include <jmorecfg.h>
#include "opendavinci/odcore/data/TimeStamp.h"

namespace msv
{

using namespace odcore::data;
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

RotatedRect bigRect;
bool intersectionOn = false;
bool foundIntersection = false;
RoadState roadState = NORMAL;
int intersectionRect;
bool calcIntersectionGoalLine = false;
int intersection_start;

LineDetector::LineDetector(const Mat &f, const Config &cfg, const bool debug,
                           const int id) :
    m_frame(), m_frameCanny(), m_lines(NULL), m_debug(debug), m_lastSolidRightTop(), detectedLines(), m_config(
        cfg), confidenceLevel(0)
{
    if(m_debug)
        cout << "___start LineDetector" << endl;
    m_frame = f.clone();
    //if (m_debug)
    //imshow("m_frame",m_frame);
    w = m_frame.size().width;
    h = m_frame.size().height;
    offset = 2 * h / 16 - 1;
    /// Detect edges using Threshold
//    threshold(m_frame, m_frame, 0, 255, CV_THRESH_BINARY + CV_THRESH_OTSU);
//    threshold(m_frame, m_frame, m_config.th1, 255, CV_THRESH_BINARY);

    TimeStamp currentTime;
    startTime = currentTime.toMicroseconds();

    threshold(m_frame, m_frame, 140, 255, CV_THRESH_BINARY);

    TimeStamp endTime;
    time_taken_threshold = endTime.toMicroseconds() - startTime;

    if (m_debug)
    cvtColor(m_frame, m_frame_color, CV_GRAY2BGR);

    // Run lineDetector and provide goalLines for the driver
    findLines();
    //cout << "Id:" << id << endl;

    if(m_debug)
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

void LineDetector::extractRoad() {
    linesContour.reset();

    struct LinesStartPos {
        Point rightLine = Point(-1, -1);
        Point firstDash = Point(-1, -1);
        Point secondDash = Point(-1, -1);
        Point leftLine = Point(-1, -1);
    } linesStartPos;

    enum LineState {
        NO_LINE_FOUND,
        LINE_FOUND,
        LINE_PASSED
    };

    enum DashState {
        NO_DASH_FOUND,
        FIRST_DASH_START,
        FIRST_DASH_END,
        SECOND_DASH_START,
        SECOND_DASH_END
    };

    int maxWidth = m_frame.cols - 1;
    int halfWidth = maxWidth / 2;
    int maxHeight = m_frame.rows - 1;
    int halfHeight = maxHeight / 2;
    int scanOffset = 3;

    int rightLineScan = halfWidth + halfWidth / 5;
    int dashLineScan = halfWidth - halfWidth / 5;
    int leftLineScan = -1;
    auto dashState = NO_DASH_FOUND;
    int whitesCount = 0;
    int maxWhites = 30;
    int lastFirstLinePoint = -1;
    int minLineHeight = 30;

    uchar* framePointer;

    bool breakLoop = false;
    bool breakRightLine = false;
    bool breakLeftLine = false;

    for (int row = maxHeight; row >= 0; row--) {
        if (breakLoop) break;

        auto rightLineState = NO_LINE_FOUND;
        auto dashLineState = NO_LINE_FOUND;
        auto leftLineState = NO_LINE_FOUND;

        framePointer = m_frame.ptr<uchar>(row);

        // scan right line
        for (int col = rightLineScan; col <= maxWidth; col++) {
            if (breakRightLine) break;

            uchar color = framePointer[col];

            if (color == 255) {
                if (whitesCount++ > maxWhites) break;

                if (rightLineState == NO_LINE_FOUND) {
                    rightLineScan = col - scanOffset;
                    rightLineState = LINE_FOUND;

                    if (linesStartPos.rightLine.x < 0) linesStartPos.rightLine = Point(col, row);
                    linesContour.rightLine.push_back(Point(col, row));
                }
            } else {
                if (whitesCount) whitesCount = 0;

                if (linesStartPos.rightLine.x > 0 && rightLineState == NO_LINE_FOUND) {
                    if (col > rightLineScan + scanOffset + 1) {
                        // stop scanning for the right line while keeping its last position for the dash line
                        breakRightLine = true;

                        break;
                    }
                }

                if (rightLineState == LINE_FOUND) {
                    rightLineState = LINE_PASSED;
                } else if (rightLineState == LINE_PASSED) {
                    linesContour.rightLine.push_back(Point(col, row));

                    break;
                }
            }
        }

        whitesCount = 0;

        // scan left line
        if (row < halfHeight && leftLineScan == -1) leftLineScan = 0;

        for (int col = leftLineScan; col >= 0; col--) {
            if (breakLeftLine) break;

            uchar color = framePointer[col];

            if (color == 255) {
                if (whitesCount++ > maxWhites) break;

                if (leftLineState == NO_LINE_FOUND) {
                    leftLineState = LINE_FOUND;
                    leftLineScan = col + scanOffset;

                    if (linesStartPos.leftLine.x < 0) linesStartPos.leftLine = Point(col, row);
                    linesContour.leftLine.push_back(Point(col, row));
                }
            } else {
                if (whitesCount) whitesCount = 0;

                if (linesStartPos.leftLine.x > 0 && leftLineState == NO_LINE_FOUND) {
                    int limit = col < leftLineScan + scanOffset + 1;
                    if (limit < 0) limit = 1;

                    if (col < limit) {
                        // stop scanning for the right line while keeping its last position for the dash line
                        breakRightLine = true;

                        break;
                    }
                }

                if (leftLineState == LINE_FOUND) {
                    leftLineState = LINE_PASSED;
                } else if (leftLineState == LINE_PASSED) {
                    linesContour.leftLine.push_back(Point(col, row));

                    break;
                }
            }
        }

        whitesCount = 0;

        // scan dash line
        for (int col = dashLineScan; col >= 0; col--) {
            if (row < halfHeight - halfHeight / 5 && linesStartPos.firstDash.x < 0) dashLineScan = -1;

            // stop scanning if it touches one of the solid lines
            if (linesStartPos.leftLine.x > -1 && col <= leftLineScan) break;
            if (linesStartPos.rightLine.x > -1 && col >= rightLineScan) break;

            uchar color = framePointer[col];

            if (color == 255) {
                if (dashLineState == NO_LINE_FOUND) {
                    dashLineState = LINE_FOUND;

                    dashLineScan = col + scanOffset - 1;

                    if (dashState == NO_DASH_FOUND) {
                        dashState = FIRST_DASH_START;

                        if (linesStartPos.firstDash.x < 0) linesStartPos.firstDash = Point(col, row);
                    } else if (dashState == FIRST_DASH_END) {
                        dashState = SECOND_DASH_START;

                        if (linesStartPos.secondDash.x < 0) linesStartPos.secondDash = Point(col, row);
                    } else if (dashState == SECOND_DASH_START && ! row) {
                        dashState = SECOND_DASH_END;
                    }

                    if (dashState == FIRST_DASH_START) {
                        linesContour.firstDash.push_back(Point(col, row));
                    } else if (dashState == SECOND_DASH_START) {
                        linesContour.secondDash.push_back(Point(col, row));
                    }

                    if (! col) lastFirstLinePoint = 0;
                }
            } else {
                if (dashLineState == LINE_FOUND) {
                    dashLineState = LINE_PASSED;

                    lastFirstLinePoint = col - maxWhites;
                    if (lastFirstLinePoint < 0) lastFirstLinePoint = 0;
                } else if (dashLineState == NO_LINE_FOUND && col == lastFirstLinePoint) {
                    dashLineState = LINE_PASSED;

//                    dashLineScan = lastFirstLinePoint + 150;
                    dashLineScan += scanOffset + 1;

                    if (dashState == FIRST_DASH_START) {
                        // checks if the dash is tall enough
                        if (linesStartPos.firstDash.y - row > minLineHeight) {
                            dashState = FIRST_DASH_END;
                        } else {
                            linesStartPos.firstDash = Point(-1, -1);
                            linesContour.firstDash = {};
                            dashState = NO_DASH_FOUND;
                        }
                    } else if (dashState == SECOND_DASH_START) {
                        dashState = SECOND_DASH_END;

                        breakLoop = true;
                    }
                } else if (dashLineState == LINE_PASSED) {
                    if (dashState == FIRST_DASH_START) {
                        linesContour.firstDash.push_back(Point(col, row));
                    } else if (dashState == SECOND_DASH_START) {
                        linesContour.secondDash.push_back(Point(col, row));
                    }

                    break;
                }
            }
        }
    }
}

float LineDetector::getRealAngle(RotatedRect rectangle) {
    if (rectangle.size.width < rectangle.size.height){
        return rectangle.angle + 180;
    } else {
        return rectangle.angle + 90;
    }
}

bool LineDetector::extractLine(vector<Point> line, int minArea, int index, CustomLine &lineContainer) {
    if (! line.size()) return false;

    RotatedRect rect;
    rect = minAreaRect(line);

    if (rect.size.area() < minArea) return false;

    if (rect.center.y < 60 && rect.center.x < 60) return false; // abort if rectangle is in the left most corner

    float realAngle;

    realAngle = getRealAngle(rect);

    if (realAngle >= 80 && realAngle <= 120) return false; // remove horizontal lines

    Point2f rectPoints[4];
    rect.points(rectPoints);

    int sizeX = 0;
    int sizeY = 0;
    int sizeR = 0;
    Point rectCenter;
    Point shortSideMiddle;
    Point longSideMiddle;

    for (int j = 0; j < 4; j++) {
        sizeR = (int) cv::sqrt(cv::pow((rectPoints[j].x - rectPoints[(j + 1) % 4].x), 2)
                               + cv::pow((rectPoints[j].y - rectPoints[(j + 1) % 4].y), 2));

        if (! sizeX) {
            sizeX = sizeR;
            shortSideMiddle.x = (int) ((rectPoints[j].x + rectPoints[(j + 1) % 4].x) / 2);
            shortSideMiddle.y = (int) ((rectPoints[j].y + rectPoints[(j + 1) % 4].y) / 2);
        } else if (! sizeY && sizeR != sizeX) {
            sizeY = sizeR;
            longSideMiddle.x = (int) ((rectPoints[j].x + rectPoints[(j + 1) % 4].x) / 2);
            longSideMiddle.y = (int) ((rectPoints[j].y + rectPoints[(j + 1) % 4].y) / 2);
        }
    } if (sizeX > sizeY) {
        Point2f temp;
        sizeR = sizeX;
        sizeX = sizeY;
        sizeY = sizeR;
        temp = longSideMiddle;
        shortSideMiddle = temp;
    }

    rectCenter.x = (int) rect.center.x;
    rectCenter.y = (int) rect.center.y;

    rect.angle = getLineSlope(shortSideMiddle, rectCenter);

    lineContainer = createLineFromRect(&rect, sizeX, sizeY, index);
    lineContainer.center.x = (int) rect.center.x;
    lineContainer.center.y = (int) rect.center.y;

    return true;
}

void LineDetector::extractLines() {
    CustomLine firstDash;
    CustomLine secondDash;
    CustomLine rightLine;
    CustomLine leftLine;

    bool rightLineFound = false;
    bool leftLineFound = false;
    bool firstDashFound = false;
    bool secondDashFound = false;

    firstDashFound = extractLine(linesContour.firstDash, 200, 1, firstDash) && firstDash.center.x <= 375;

    if (firstDashFound) {
        ltu.foundD = true;
        ltu.dashedCurveFound = true;
        ltu.dashedCurve.push_back(firstDash);

        secondDashFound = extractLine(linesContour.secondDash, 200, 2, secondDash);

        if (secondDashFound) {
            int distance = (int) norm(firstDash.center - secondDash.center);

            if (distance < 300) {
                ltu.dashedCurve.push_back(secondDash);
            }
        }
    }

    leftLineFound = extractLine(linesContour.leftLine, 400, 3, leftLine);

    if (leftLineFound) {
        ltu.leftLine = leftLine;
        ltu.foundL = true;
    } else {
        ltu.foundL = false;
    }

    rightLineFound = extractLine(linesContour.rightLine, 1500, 0, rightLine);

    if (rightLineFound) {
        ltu.rightLine = rightLine;
        ltu.foundR = true;
    }

    int primaryLaneSizeMin = 200;
    int parimaryLaneSizeMax = 670;
    int primaryLaneScale = parimaryLaneSizeMax - primaryLaneSizeMin;
    float primaryLaneRatio = (float) (primaryLaneScale * 1.0 / m_frame.rows);

    int secondaryLaneSizeMin = 110;
    int secondaryLaneSizeMax = 320;
    int secondaryLaneScale = secondaryLaneSizeMax - secondaryLaneSizeMin;
    float secondaryLaneRatio = (float) (secondaryLaneScale * 1.0 / m_frame.rows);

    if (! firstDashFound) {
        // attempt to reconstruct the dash line using the right line
        if (rightLineFound) {
            firstDash.p1.x = (int) (rightLine.p1.x - primaryLaneSizeMin - rightLine.p1.y * primaryLaneRatio);
            firstDash.p1.y = rightLine.p1.y;
            firstDash.p2.x = (int) (rightLine.p2.x - primaryLaneSizeMin - rightLine.p2.y * primaryLaneRatio);
            firstDash.p2.y = rightLine.p2.y;
            firstDash.slope = getLineSlope(firstDash.p1, firstDash.p2);

            ltu.foundD = true;
            ltu.dashedCurveFound = true;
            ltu.dashedCurve.push_back(firstDash);
        } else if (leftLineFound) {
            firstDash.p1.x = (int) (leftLine.p1.x + secondaryLaneSizeMin + leftLine.p1.y * secondaryLaneRatio);
            firstDash.p1.y = leftLine.p1.y;
            firstDash.p2.x = (int) (leftLine.p2.x + secondaryLaneSizeMin + leftLine.p2.y * secondaryLaneRatio);
            firstDash.p2.y = leftLine.p2.y;
            firstDash.slope = getLineSlope(firstDash.p1, firstDash.p2);

            ltu.foundD = true;
            ltu.dashedCurveFound = true;
            ltu.dashedCurve.push_back(firstDash);

            firstDashFound = true;
        } else {
            ltu.foundD = false;
            ltu.dashedCurveFound = false;
        }
    }

    if (! rightLineFound) {
        // attempt to reconstruct the right line using dashed lines

        //TODO: extract this into a function
        if (firstDashFound && secondDashFound) {
            rightLine.p1.x = (int) (firstDash.p1.x + primaryLaneSizeMin + firstDash.p1.y * primaryLaneRatio);
            rightLine.p1.y = firstDash.p1.y;
            rightLine.p2.x = (int) (secondDash.p2.x + primaryLaneSizeMin + secondDash.p2.y * primaryLaneRatio);
            rightLine.p2.y = secondDash.p2.y;
            rightLine.slope = getLineSlope(rightLine.p1, rightLine.p2);

            ltu.rightLine = rightLine;
            ltu.foundR = true;
        } else if (firstDashFound && ! secondDashFound) {
            rightLine.p1.x = (int) (firstDash.p1.x + primaryLaneSizeMin + firstDash.p1.y * primaryLaneRatio);
            rightLine.p1.y = firstDash.p1.y;
            rightLine.p2.x = (int) (firstDash.p2.x + primaryLaneSizeMin + firstDash.p2.y * primaryLaneRatio);
            rightLine.p2.y = firstDash.p2.y;
            rightLine.slope = getLineSlope(rightLine.p1, rightLine.p2);

            ltu.rightLine = rightLine;
            ltu.foundR = true;
        } else {
            ltu.foundR = false;
        }
    }
}

// The "body"
void LineDetector::findLines()
{
    if (m_debug)
    {
        TimeStamp currentTime;
        startTime = currentTime.toMicroseconds();
    }

    extractRoad();

    if (m_debug)
        {
            TimeStamp endTime;
            time_taken_extractRoad = endTime.toMicroseconds() - startTime;
            TimeStamp currentTime;
            startTime = currentTime.toMicroseconds();
        }

    extractLines();

    if (m_debug)
    {
        TimeStamp endTime;
        time_taken_extractLines = endTime.toMicroseconds() - startTime;
        TimeStamp currentTime;
        startTime = currentTime.toMicroseconds();
    }

//    //Find contours
//    getContours();
//
//    if (m_debug)
//        {
//            TimeStamp endTime;
//            time_taken_contour = endTime.toMicroseconds() - startTime;
//            result_getContours.contours = contours_poly;
//            TimeStamp currentTime;
//            startTime = currentTime.toMicroseconds();
//        }
//    //Get all marked lines
//    getRectangles();
//    if (m_debug)
//        {
//            TimeStamp endTime;
//            time_taken_find_lines = endTime.toMicroseconds() - startTime;
//            result_getRectangles.rects = rects;
//            TimeStamp currentTime;
//            startTime = currentTime.toMicroseconds();
//        }
//
//    //Classify dash lines and solid lines
//    classification();
//    if (m_debug)
//        {
//            TimeStamp endTime;
//            time_taken_classification = endTime.toMicroseconds() - startTime;
//            result_classification.dashLines = dashLines;
//            result_classification.solidLines = solidLines;
//            result_classification.cntDash = cntDash;
//            result_classification.cntSolid = cntSolid;
//            result_classification.foundStopStartLine = foundStopStartLine;
//            result_classification.intersectionOn = intersectionOn;
//            result_classification.foundIntersection = foundIntersection;
//            TimeStamp currentTime;
//            startTime = currentTime.toMicroseconds();
//        }
//
//    characteristicFiltering(&ltu);
//    if (m_debug)
//        {
//            TimeStamp endTime;
//            time_taken_characteristicFiltering = endTime.toMicroseconds() - startTime;
//            // Debug data is not provided for this function. It is sufficient to look
//            // at the debug data from createTrajectory
//        }

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
//    createIntersectionGoalLine();
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

void LineDetector::getContours()
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

    for (unsigned int i = 0; i < contours.size(); i++)
        {
            approxPolyDP(Mat(contours[i]), contours_poly[i], 3, true);
        }

    return;
}

void LineDetector::getRectangles()
{
    RotatedRect rect;

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
    bool printouts = false;
    int sizeX;
    int sizeY;
    int area;
    RotatedRect rect;
    Point2f rect_points[4];
    Point rectCenter;
    Point shortSideMiddle;
    intersectionRect = -1;

    bool intersectionLineFirst = false;
    bool intersectionLineSecond = false;

    int rightLineDist = -1;
    int rightLineIndex = -1;
    int firstDashDist = -1;
    int firstDashIndex = -1;
    int secondDashDist = -1;
    int secondDashIndex = -1;
    int leftLineDist = -1;
    int leftLineIndex = -1;
    int distance;

    for (int i = 0; i < rects.size(); i++) {
        if (roadState == INTERSECTION) break;

        rect = rects[i];
        sizeX = line_sizes[i].sizeX;
        sizeY = line_sizes[i].sizeY;
        area = sizeX * sizeY;
        rectCenter.x = (int) rect.center.x;
        rectCenter.y = (int) rect.center.y;
        shortSideMiddle = line_sizes[i].shortSideMiddle;

        if (rect.size.width < 2) continue; // filter out garbage

        /**
         * Detect intersection.
         */
        if (area > m_config.maxArea * 10000) { // Detect stop line.
            roadState = INTERSECTION;
            intersectionOn = true;
            calcIntersectionGoalLine = true;
            foundIntersection = true;
            intersectionType = STOP_LINE_INTERSECTION;
            intersection_start = m_config.currentDistance;

            cout << "STOP LINE INTERSECTION" << endl;

            break;
        } else { // Detect intersection with no stop line/sideways stop line.
            if (! intersectionLineFirst) {
                if (rect.center.x > 375 && rect.center.y > 125 && rect.angle < 10) {
                    if (rect.size.height > 200) {
                        intersectionLineFirst = true;
                    }
                }
            } else if (! intersectionLineSecond) {
                if (rect.center.x > 375 && rect.center.y < 125 && rect.angle < 10) {
                    if (rect.size.height > 50) {
                        intersectionLineSecond = true;
                    }
                }
            }

            if (intersectionLineFirst && intersectionLineSecond) {
                roadState = INTERSECTION;
                intersectionOn = true;
                calcIntersectionGoalLine = true;
                foundIntersection = true;
                intersection_start = m_config.currentDistance;
                intersectionType = INTERSECTION_WITHOUT_STOP_LINE;

                cout << "INTERSECTION NO STOP LINE" << endl;

                break;
            }
        }

        /**
         * Proceed checking for solid and dashed lines if no intersection.
         */
        rects[i].angle = getLineSlope(shortSideMiddle, rectCenter);

//        if (linesApproxPos.rightLine.x > -1) {
//            distance = (int) norm(linesApproxPos.rightLine - rectCenter);
//
//            if (rightLineDist < 0 || distance < rightLineDist) {
//                rightLineDist = distance;
//                rightLineIndex = i;
//            }
//        }
//
//        if (linesApproxPos.firstDash.x > -1) {
//            distance = (int) norm(linesApproxPos.firstDash - rectCenter);
//
//            if (firstDashDist < 0 || distance < firstDashDist) {
//                firstDashDist = distance;
//                firstDashIndex = i;
//            }
//        }
//
//        if (linesApproxPos.secondDash.x > -1) {
//            distance = (int) norm(linesApproxPos.secondDash - rectCenter);
//
//            if (secondDashDist < 0 || distance < secondDashDist) {
//                secondDashDist = distance;
//                secondDashIndex = i;
//            }
//        }
//
//        if (linesApproxPos.leftLine.x > -1) {
//            distance = (int) norm(linesApproxPos.leftLine - rectCenter);
//
//            if (leftLineDist < 0 || distance < leftLineDist) {
//                leftLineDist = distance;
//                leftLineIndex = i;
//            }
//        }
    }

    /**
     * Assign dashed and solid lines if available.
     */
    if (rightLineIndex > -1) {
        rect = rects[rightLineIndex];
        sizeX = line_sizes[rightLineIndex].sizeX;
        sizeY = line_sizes[rightLineIndex].sizeY;
        solidLines.push_back(createLineFromRect(&rect, sizeX, sizeY, rightLineIndex));
    }

    if (leftLineIndex > -1) {
        rect = rects[leftLineIndex];
        sizeX = line_sizes[leftLineIndex].sizeX;
        sizeY = line_sizes[leftLineIndex].sizeY;
        solidLines.push_back(createLineFromRect(&rect, sizeX, sizeY, leftLineIndex));
    }

    if (firstDashIndex > -1) {
        rect = rects[firstDashIndex];
        sizeX = line_sizes[firstDashIndex].sizeX;
        sizeY = line_sizes[firstDashIndex].sizeY;
        dashLines.push_back(createLineFromRect(&rect, sizeX, sizeY, firstDashIndex));
    }

    if (secondDashIndex > -1) {
        rect = rects[secondDashIndex];
        sizeX = line_sizes[secondDashIndex].sizeX;
        sizeY = line_sizes[secondDashIndex].sizeY;
        dashLines.push_back(createLineFromRect(&rect, sizeX, sizeY, secondDashIndex));
    }

    /**
     * Handle intersection state.
     */
    int distanceTravelled = m_config.currentDistance - intersection_start;
    int distanceToTravel = 0;

    if (intersectionType == INTERSECTION_WITHOUT_STOP_LINE) {
        distanceToTravel = 80;
    } else if (intersectionType == STOP_LINE_INTERSECTION) {
        distanceToTravel = 120;
    }

    if (intersectionType && distanceTravelled > distanceToTravel){
        roadState = NORMAL;
        intersectionOn = false;
        foundIntersection = false;
        calcIntersectionGoalLine = false;
        intersectionType = NO_INTERSECTION;
    }
}

void LineDetector::characteristicFiltering(LinesToUse *ltu) {
    CustomLine line;

    int middleOfScreen = m_frame.cols / 2;

    ltu->foundR = false;
    ltu->foundL = false;
    ltu->foundD = false;
    ltu->dashedCurveFound = false;

    for (int i = 0; i < solidLines.size(); i++) {
        line = solidLines[i];

        if (line.p1.x > middleOfScreen) {
            ltu->rightLine = line;
            ltu->foundR = true;
        } else {
            ltu->leftLine = line;
            ltu->foundL = true;
        }
    }

    if (dashLines.size()) {
        sort(dashLines.begin(), dashLines.end());

        ltu->foundD = true;
        ltu->dashedCurve = dashLines;
        ltu->dashedCurveFound = true;
    }
}

void LineDetector::createTrajectory(LinesToUse *ltu)
{
    if (!(ltu->foundL || ltu->foundD || ltu->foundR) || roadState == INTERSECTION) {
        cout << "No lines found or INTERSECTION mode, trajectory will not be derived." << endl;

        finalOutput.noTrajectory = true;
        finalOutput.intersection_goalLine = false;
        dataToDriver = new LaneDetectorDataToDriver(); // Empty call will set noTrajectory = true
        return;
    }

    EstimationData ed;
    GoalLineData gld;

    if (ltu->foundR) {
        ed.right = ltu->rightLine;
    } else {
        ed.right = getNoneCustomLine();
    }

    if (ltu->foundL) {
        ed.left = ltu->leftLine;
    } else {
        ed.left = getNoneCustomLine();
    }

    if (ltu->foundD) {
        if (ltu->dashedCurve.size() > 1) {
            (ltu->dashedCurve)[0].p2.x = (ltu->dashedCurve)[1].p2.x;
            (ltu->dashedCurve)[0].p2.y = (ltu->dashedCurve)[1].p2.y;
            (ltu->dashedCurve)[0].slope = getLineSlope(
                    (ltu->dashedCurve)[0].p1,
                    (ltu->dashedCurve)[0].p2
            );
        }

        ed.dash = ltu->dashedCurve[0];
    } else {
        ed.dash = getNoneCustomLine();
    }

    ed.yPosition = (2 * h) / 3;

    provideGoalLine(&ed, &gld);

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

    dataToDriver = new LaneDetectorDataToDriver(
            gld.leftGoalLine,
            gld.rightGoalLine,
            currentLine,
            false,
            gld.confidenceLevel_rightGoalLine
    );

    if (m_debug) {
        finalOutput.estimatedLeft = {};
        finalOutput.estimatedLeft.push_back(ed.isLeftEstimated);
        finalOutput.estimatedDash = {};
        finalOutput.estimatedDash.push_back(ed.isDashEstimated);
        finalOutput.estimatedRight = {};
        finalOutput.estimatedRight.push_back(ed.isRightEstimated);
        finalOutput.left = {};
        finalOutput.left.push_back(ed.left);
        finalOutput.dash = {};
        finalOutput.dash.push_back(ed.dash);
        finalOutput.right = {};
        finalOutput.right.push_back(ed.right);
        finalOutput.leftGoalLines = {};
        finalOutput.leftGoalLines.push_back(gld.leftGoalLine);
        finalOutput.rightGoalLines = {};
        finalOutput.rightGoalLines.push_back(gld.rightGoalLine);
        finalOutput.currentLine = currentLine;
        finalOutput.noTrajectory = false;
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

void LineDetector::createIntersectionGoalLine(){
    bool printouts = false;
    if (printouts && m_debug)
        cout << "__start createIntersectionGoalLine" << endl;

    CustomLine newRightGoalLine = getNoneCustomLine();
    CustomLine newLeftGoalLine = getNoneCustomLine();
    if (roadState == INTERSECTION){
        dataToDriver->leftGoalLines0 = getNoneCustomLine();
        dataToDriver->rightGoalLines0 = getNoneCustomLine();

        if (intersectionRect != -1 && calcIntersectionGoalLine == true){
            if (printouts && m_debug)
                cout << "Check if to calculate new goalLine based on the intersection rectangle" << endl;
            Point rectCenter;
            Point2f rect_points[4];
            bigRect.points(rect_points);
            PolySize p = createPolySize(bigRect);
            // Calc rect width
            int rect_width = 0;
            for (int i = 0; i < 3; i++){
                int now_width = abs(rect_points[i].x - rect_points[i+1].x);
                if (now_width > rect_width)
                    rect_width = now_width;
            }
            // Calc if rect intersects current line
            int xCut = w/2.;
            bool rectXBigger = false, rectXSmaller = false;
            Point dashPoint;
            int dashXPos = w;
            for (int i = 0; i < 4; i++){

            if (printouts && m_debug)
                cout << "rect_points[i]: " << rect_points[i] << endl;

                if (rect_points[i].x < dashXPos){
                    dashXPos = rect_points[i].x;
                    dashPoint = rect_points[i];
                }

                if(rect_points[i].x < xCut)
                    rectXSmaller = true;
                else if (rect_points[i].x > xCut)
                    rectXBigger = true;
            }
            if (printouts && m_debug){
                cout << "rectXBigger: " << rectXBigger << endl;
                cout << "rectXSmaller: " << rectXSmaller << endl;
                cout << "rect_width: " << rect_width << endl;
                cout << "YI: " << YI << endl;
                cout << " 0.95*w: " <<  0.95*w << endl;
                cout << "intersectionRect: " << intersectionRect << endl;
            }

            if (rectXBigger && rectXSmaller && rect_width < 0.95*w && YI > h/2){
                rectCenter.x = bigRect.center.x;
                rectCenter.y = bigRect.center.y;
                float rectSlope = getLineSlope(p.longSideMiddle, rectCenter);
                newRightGoalLine.p1 = dashPoint;
                newRightGoalLine.p1.x += ROAD_SIZE/2;
                newRightGoalLine.slope = rectSlope - 2*(rectSlope - 90);
                newRightGoalLine.p2.x = getIntersectionWithBottom(newRightGoalLine);
                newRightGoalLine.p2.y = h;
                newRightGoalLine.p1.x = getIntersectionWithTop(newRightGoalLine);
                newRightGoalLine.p1.y = 0;
                // Derive left goal line
                newLeftGoalLine = newRightGoalLine;
                newLeftGoalLine.slope = abs(newRightGoalLine.slope) + ROAD_ANGLE*0.65;// 2*(90 - ROAD_ANGLE);//180 - abs(newRightGoalLine.slope) - ROAD_ANGLE;
                newLeftGoalLine.p2.x = newRightGoalLine.p2.x - ROAD_SIZE;
                newLeftGoalLine.p1.x = getIntersectionWithTopP2(newLeftGoalLine);
                if (printouts && m_debug){
                    cout << "changes to new goalLine!" << endl;
                    cout << "newRightGoalLine slope: " << newRightGoalLine.slope << " p1(" << newRightGoalLine.p1.x << "," << newRightGoalLine.p1.y;
                    cout << ") p2(" << newRightGoalLine.p2.x << "," << newRightGoalLine.p2.y << ")" << endl;
                    cout << "newLeftGoalLine slope: " << newLeftGoalLine.slope << " p1(" << newLeftGoalLine.p1.x << "," << newLeftGoalLine.p1.y;
                    cout << ") p2(" << newLeftGoalLine.p2.x << "," << newLeftGoalLine.p2.y << ")" << endl;
                }
            }
        }
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
        if (m_debug){
            std::vector<CustomLine> rightGoalLines;
            rightGoalLines.push_back(newRightGoalLine);
            finalOutput.rightGoalLines = rightGoalLines;
            std::vector<CustomLine> leftGoalLines;
            leftGoalLines.push_back(newLeftGoalLine);
            finalOutput.leftGoalLines = leftGoalLines;
            finalOutput.noTrajectory = true;
            finalOutput.currentLine = currentLine;

            if(isNoneCustomLine(newRightGoalLine))
                finalOutput.intersection_goalLine = false;
            else
                finalOutput.intersection_goalLine = true;
        }

        if(isNoneCustomLine(newRightGoalLine)){
            dataToDriver->noTrajectory = true;
        }else{
            dataToDriver->noTrajectory = false;
            dataToDriver->rightGoalLines0 = newRightGoalLine;
            dataToDriver->leftGoalLines0 = newLeftGoalLine;
            dataToDriver->currentLine = currentLine;
        }
    }
    if (printouts && m_debug)
        cout << "__end createIntersectionGoalLine" << endl;
}

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
                    // OLD WAY
                    gld->rightGoalLine = gld->leftGoalLine;
                    gld->rightGoalLine.p2.x += ed->calcRoadSize;
                    gld->rightGoalLine.slope = getLineSlope(gld->rightGoalLine.p2, gld->rightGoalLine.p1);
                    gld->confidenceLevel_rightGoalLine = 4;
                    // NEW WAY
                    // int expectedRightLineX = getIntersectionWithY(ed->dash, ed->yPosition) + ed->calcRoadSize;
                    // float expectedRightLineAngle = 180 - abs(ed->dash.slope)
                    //                                - calcRoadAngle;
                    // if (printouts)
                    //     {
                    //         cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
                    //         cout << "calcRoadAngle: " << calcRoadAngle << endl;
                    //         cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
                    //         cout << "abs(ed->dash.slope): " << abs(ed->dash.slope) << endl;
                    //     }

                    // if (expectedRightLineAngle > 90)
                    //     {
                    //         expectedRightLineAngle = expectedRightLineAngle - 180;
                    //     }
                    // if (printouts)
                    //     cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
                    // ed->right.slope = expectedRightLineAngle;
                    // ed->right.p1.x = expectedRightLineX;
                    // ed->right.p1.y = ed->yPosition;
                    // ed->isRightEstimated = true;

                    // // Calculate right goal line
                    // gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
                    // gld->confidenceLevel_rightGoalLine = 3;
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
                    //float distInBetween = getDist(lines[0].p2, lines[j].p1);

                    if (printouts)
                        {
                            cout << "slopeA: " << slopeA;
                            cout << " slopeB: " << slopeB;
                            cout << " slopeInBetween: " << slopeInBetween << endl;
                            //cout << " distInBetween: " << distInBetween << endl;
                            cout << " slopeDiffLines: " << slopeDiffLines;
                            cout << " slopeDiffToLine0: " << slopeDiffToLine0;
                            cout << " slopeDiffToLineJ: " << slopeDiffToLineJ << endl;
                        }

                    if ((slopeDiffLines < 60) && (slopeDiffToLine0 < slopeDiffLines + 20) &&
                            (slopeDiffToLineJ < slopeDiffLines + 20))// && (distInBetween < m_config.maxY * 0.7))
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
int LineDetector::getIntersectionWithTopP2(CustomLine l) const
{
    float a = tan(M_PI * l.slope / 180);
    float b = l.p2.y - l.p2.x * a;
    int positionX = l.p2.x;
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



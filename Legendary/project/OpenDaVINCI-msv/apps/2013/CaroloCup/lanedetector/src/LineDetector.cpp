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

namespace carolocup
{

using namespace std;
using namespace cv;

int cntDash = 0;
int cntSolid = 0;
vector<CustomLine> dashLines;
vector<CustomLine> solidLines;
int h, w, offset;
int roadSize = ROAD_SIZE;
int roadAngle = 91;
bool foundStopStartLine = false;
int oldDashGoalX = 0;
int oldRightGoalX = 0;
int oldLeftGoalX = 0;
int calcRoadSize, calcRoadAngle;

LineDetector::LineDetector(const Mat& f, const Config& cfg, const bool debug, const int id)
    : m_lines(NULL)
    , m_debug(debug)
    , m_lastSolidRightTop()
    , detectedLines()
    , m_frame()
    , m_frameCanny()
    , m_config(cfg)
{
    m_frame = f.clone();
    Mat outputImg = f.clone();
    //if (m_debug)
    //imshow("m_frame",m_frame);
    w = m_frame.size().width;
    h = m_frame.size().height;
    offset = 3*h/16 - 1;
    /// Detect edges using Threshold
    threshold( m_frame, m_frame, cfg.th1, 255, CV_THRESH_BINARY);

    //Find dash and solid lines
    findLines(outputImg);

    /*Point2i pm;
    pm.x = 752/2;
    pm.y = 480;
    Point2f wpm = getWorldPoint(pm);
    cout << "World mid point: " << wpm.x << "," << wpm.y << endl;
    pm.x = 0;
    pm.y = 480;
    wpm = getWorldPoint(pm);
    cout << "World mid point: " << wpm.x << "," << wpm.y << endl;
    pm.x = 752;
    pm.y = 480;
    wpm = getWorldPoint(pm);
    cout << "World mid point: " << wpm.x << "," << wpm.y << endl;
    pm.x = 0;
    pm.y = 0;
    wpm = getWorldPoint(pm);
    cout << "World mid point: " << wpm.x << "," << wpm.y << endl;
    pm.x = 752/2;
    pm.y = 480/2;
    wpm = getWorldPoint(pm);
    cout << "World mid point: " << wpm.x << "," << wpm.y << endl;*/


    // View
    if(m_debug) imshow("Output", outputImg);
}

LineDetector::~LineDetector()
{
    if (NULL != m_lines)
    {
        delete m_lines;
    }
}

Lines LineDetector::getLines()
{
    if (NULL == m_lines)
    {
	//ofstream mylog;
	//mylog.open("test.log", ios::out | ios::app);
	Vec4i dashLine(0,0,0,0), leftLine(0,0,0,0), rightLine(0,0,0,0);

	bool foundR=false, foundL=false, foundD=false, shrinkSize=false;
	CustomLine supDashLine, supRightLine, supLeftLine;
        //Pick the suitable dashLine
	cout << "Pick lines!" << endl;
	if(cntDash > 0) {
		supDashLine.p1.y = 0;
		supDashLine.p2.y = 0;
        	std::sort(dashLines.begin(), dashLines.begin() + cntDash);
		//bool isInvertedD = false;
		//bool discardInverts = false;
		for(int i = 0; i < cntDash; i++) {
		    //if(dashLines[i].slope < supDashLine.slope) {
		    /*if(!discardInverts && dashLines[i].slope < 0) {
                        discardInverts = true;
			i = 0;
			cout << "No inverts" << endl;
		    }*/
		    //cout << "Dash y: " << max(dashLines[i].p1.y,dashLines[i].p2.y) << endl;
		    //cout << "Dash max: " << max(dashLines[i+1].p1.y,dashLines[i+1].p2.y) << " Dash min: " <<  min(dashLines[i].p1.y,dashLines[i].p2.y) << endl;
		    if(i != cntDash - 1 && max(dashLines[i+1].p1.y,dashLines[i+1].p2.y) > min(dashLines[i].p1.y,dashLines[i].p2.y)) {
			cout << "Removing wrong dash!" << endl;
			int positionX = getIntersectionWithBottom(dashLines[i]);
			int nPositionX = getIntersectionWithBottom(dashLines[i+1]);
			//cout << "Curr closenest: " << abs(oldDashGoalX - positionX) << ", other:" << abs(oldDashGoalX - nPositionX) << endl;
			if(abs(oldDashGoalX - positionX) < abs(oldDashGoalX - nPositionX)) {
			    dashLines[i+1] = dashLines[cntDash-1];
			    cntDash--;
			} else {
			    dashLines[i] = dashLines[cntDash-1];
			    cntDash--;
			}
			i--;
		    }
		    //if(max(dashLines[i].p1.y,dashLines[i].p2.y) > max(supDashLine.p1.y, supDashLine.p2.y)) {
			//supDashLine = dashLines[i];
			/*if(dashLines[i].slope > 0 && !discardInverts) {
			    isInvertedD = true;
			    supDashLine = dashLines[i];
			    cout << "Found invert" << endl;
			} else if(dashLines[i].slope < 0 ) {
			    supDashLine = dashLines[i];
			    cout << "Found" << endl;
			}*/
		    //}
		}
		supDashLine = dashLines[0];
		int dashSupPosX = getIntersectionWithBottom(supDashLine);
	        //if(supDashLine.slope < 0) {
		cout << "Dash line slope: " << supDashLine.slope << endl;
                /*Point2f dwpm1 = getWorldPoint(Point2i(supDashLine.p1.x, supDashLine.p1.y + offset));
		cout << "World dash point1: " << dwpm1.x << "," << dwpm1.y << endl;
                Point2f dwpm2 = getWorldPoint(Point2i(supDashLine.p2.x, supDashLine.p2.y + offset));
		cout << "World dash point2: " << dwpm2.x << "," << dwpm2.y << endl;
		cout << "World dash size: " << getDist(dwpm1, dwpm2)<< endl;*/
		//cout << "Dash diff: " << abs(dashSupPosX - oldDashGoalX) << endl;
		if(abs(dashSupPosX - oldDashGoalX) < calcRoadSize * 0.8 || oldDashGoalX == 0) {
			if(max(supDashLine.p1.x, supDashLine.p2.x) < w/10) {
				shrinkSize = true;
			}
			dashLine = Vec4i(supDashLine.p1.x, supDashLine.p1.y, supDashLine.p2.x, supDashLine.p2.y);
			foundD = true;
		}
	}
	//bool isInvertedR = false;
	if(cntSolid > 0) {
		supRightLine.p1.x = w;
		supRightLine.p2.x = w;
		for(int i = 0; i < cntSolid; i++) {
	     	    /*float a = tan(M_PI * solidLines[i].slope / 180);
	     	    float b = solidLines[i].p1.y - solidLines[i].p1.x * a;
	     	    float positionX = solidLines[i].p1.x;
		    if(abs(a) > 0.001) {
			positionX = (h - b) / a;
	     	    }*/
		    //if(solidLines[i].slope < 90 && solidLines[i].slope > supRightLine.slope) {
		    if(solidLines[i].slope < 90 && solidLines[i].slope > 0 && min(solidLines[i].p1.x, solidLines[i].p2.x) < min(supRightLine.p1.x, supRightLine.p2.x)){ // && max(solidLines[i].p1.x, solidLines[i].p2.x) > w/2) {
			supRightLine = solidLines[i];
			foundR = true;
		    } /*else if(!foundR && min(solidLines[i].p1.x, solidLines[i].p2.x) < min(supRightLine.p1.x, supRightLine.p2.x) && positionX > w/2) {
			supRightLine = solidLines[i];
			foundR = true;
			isInvertedR = true;
		    }*/
		}
		if(foundR) {
			int rSupPosX = getIntersectionWithBottom(supRightLine);
			//cout << "Right diff x: " << abs(rSupPosX - oldRightGoalX) << endl;
			if(abs(rSupPosX - oldRightGoalX) < calcRoadSize * 0.8 || oldRightGoalX == 0) {
				cout << "Right line slope: " << supRightLine.slope << endl;
				rightLine = Vec4i(supRightLine.p1.x, supRightLine.p1.y, supRightLine.p2.x, supRightLine.p2.y);
				oldRightGoalX = rSupPosX;
			} else {
				foundR = false;
			}
		}
		supLeftLine.p1.x = 0;
		supLeftLine.p2.x = 0;
		for(int i = 0; i < cntSolid; i++) {
		    int centerSolidLineX = (solidLines[i].p1.x + solidLines[i].p2.x)/2;
		    //if(solidLines[i].slope > -90 && solidLines[i].slope < supLeftLine.slope) {
		    if(solidLines[i].slope > -90 && solidLines[i].slope < 0 && min(solidLines[i].p1.x, solidLines[i].p2.x) > min(supLeftLine.p1.x, supLeftLine.p2.x)){ // && min(solidLines[i].p1.x, solidLines[i].p2.x) < w/2) {
			supLeftLine = solidLines[i];
			foundL = true;
		    }
		}
		if(foundL) {
			int lSupPosX = getIntersectionWithBottom(supLeftLine);
			if(abs(lSupPosX - oldLeftGoalX) < calcRoadSize * 0.8 || oldLeftGoalX == 0) {
				cout << "Left line slope: " << supLeftLine.slope << endl;
				leftLine = Vec4i(supLeftLine.p1.x, supLeftLine.p1.y, supLeftLine.p2.x, supLeftLine.p2.y);
				oldLeftGoalX = lSupPosX;
			} else {
				foundL = false;
			}
		}
	} 
	Point vp;
	Point goalP;
	//Set goal height
	goalP.y = h;
	bool foundGoal;
	//Trace different image scenarious
	int currRoadSize = calcRoadSize;
	if(foundD) {
		//We have a dash line
		float da = tan(supDashLine.slope * M_PI / 180);
		float db = supDashLine.p1.y - supDashLine.p1.x * da;
		int dashGoalX = (goalP.y - db)/da;
		oldDashGoalX = dashGoalX;
		int dashCenterX = (supDashLine.p1.x + supDashLine.p2.x)/2;
		int dashCenterY = (supDashLine.p1.y + supDashLine.p2.y)/2;
		cout << "Dash center: " << dashCenterX << "," << dashCenterY << endl;
		//cout << da << "*dx + " << db << endl;
		cout << "Dash line X: " << dashGoalX << endl;
		if(foundR) {
			//We have dash and right line
			//Calculate vanishing point
			float a = tan(supRightLine.slope * M_PI / 180);
			float b = supRightLine.p1.y - supRightLine.p1.x * a;
			//cout << a << "*x + " << b << endl;
			int rightGoalX = (goalP.y - b)/a;
			if (da != a) {
				vp.x = (b - db) / (da - a);
			}
			vp.y = da*vp.x + db;
			int roadSz =  (rightGoalX - dashGoalX);
			//mylog << supDashLine.slope << "," << dashCenterX << "," << dashCenterY << "," << roadSz << "," << (180 - abs(supDashLine.slope) - abs(supRightLine.slope)) << endl;
			calcRoadAngle = getRoadAngle(2, supDashLine.slope);
			calcRoadSize = getRoadSize(calcRoadAngle);
			goalP.x = dashGoalX + roadSz*ROAD_GOAL;//(dashGoalX + rightGoalX)/2;//dashGoalX + ROAD_SIZE/2;
			cout << "Road size: " <<  roadSz << endl;
			cout << "Road angle prediction: " << calcRoadAngle << endl;
			cout << "Road size prediction: " << calcRoadSize << endl;
			cout << "Road angle: " << (180 - abs(supDashLine.slope) - abs(supRightLine.slope)) << endl;
			cout << "Right line X: " << rightGoalX << endl;
			cout << "CASE: Dash and right" << endl;
		/*} else if(foundL){
			//We have dash and left line
			float a = tan(supLeftLine.slope * M_PI / 180);
			float b = supLeftLine.p1.y - supLeftLine.p1.x * a;
			//cout << a << "*x + " << b << endl;
			if (da != a) {
				vp.x = (b - db) / (da - a);
			}
			vp.y = da*vp.x + db;
			goalP.x = dashGoalX + ROAD_SIZE/2;
			cout << "CASE: Dash and left" << endl;*/ 
		} else {
			//We have only dash line
			//offset with half the size of road to the right
			calcRoadAngle = getRoadAngle(2, supDashLine.slope);
			double inp[3], outA[3], outS[3];
			if(supDashLine.slope < 0) {
				inp[0] = 180 + supDashLine.slope;
			} else {
				inp[0] = supDashLine.slope;
			}
			inp[1] = (supDashLine.p1.x + supDashLine.p2.x)/2;
			inp[2] = (supDashLine.p1.y + supDashLine.p2.y)/2;
			nnRoadAngleCalc(inp, outA);
			nnRoadSizeCalc(inp, outS);
			cout << "NN result: " << outA[0] << ", " << outS[0] << endl;
			calcRoadSize = getRoadSize(calcRoadAngle);
			/*if(shrinkSize) {
				calcRoadSize = 0.8 * calcRoadSize;
			}*/
			int expectedRightLineX = dashGoalX + calcRoadSize;
			float expectedRightLineAngle = 180 - abs(supDashLine.slope) - calcRoadAngle;
			if(expectedRightLineAngle > 90) {
				expectedRightLineAngle = expectedRightLineAngle - 180;
			}
			float a = tan(expectedRightLineAngle * M_PI / 180);
			float b = goalP.y - expectedRightLineX * a;
			cout << "Road angle prediction: " << calcRoadAngle << endl;
			cout << "Road size prediction: " << calcRoadSize << endl;
			cout << "Expected right line X: " << expectedRightLineX << endl;
			cout << "Expected right line angle: " << expectedRightLineAngle << endl;
			//cout << a << "*x + " << b << endl;
			if (da != a) {
				vp.x = (b - db) / (da - a);
			}
			vp.y = da*vp.x + db;
			goalP.x = dashGoalX + calcRoadSize*ROAD_GOAL;
			cout << "CASE: Only dash" << endl;
		}
		foundGoal = true;
	} else {
		//No dash line
		if(foundR) {
			//We have only right line
			//offset with half the size of road to the left
			float a = tan(supRightLine.slope * M_PI / 180);
			float b = supRightLine.p1.y - supRightLine.p1.x * a;
			int rightGoalX = (h - b)/ a;
			cout << "Right line X: " << rightGoalX << endl;
			//cout << a << "*x + " << b << endl;
			calcRoadAngle = getRoadAngle(1, supRightLine.slope);
			calcRoadSize = getRoadSize(calcRoadAngle);
			int expectedDashLineX = rightGoalX - calcRoadSize;
			float expectedDashLineAngle = 180 - abs(supRightLine.slope) - calcRoadAngle;
			if(expectedDashLineAngle > 90) {
				expectedDashLineAngle = expectedDashLineAngle - 180;
			}
			cout << "Road angle prediction: " << calcRoadAngle << endl;
			cout << "Road size prediction: " << calcRoadSize << endl;
			cout << "Expected dash line X: " << expectedDashLineX << endl;
			cout << "Expected dash line angle: " << expectedDashLineAngle << endl;
			float da = tan(expectedDashLineAngle * M_PI / 180);
			float db = goalP.y - expectedDashLineX * da;
			//cout << da << "*x + " << db << endl;
			if (da != a) {
				vp.x = (b - db) / (da - a);
			}
			vp.y = a*vp.x + b;
			foundGoal = true;
			goalP.x = expectedDashLineX + calcRoadSize*ROAD_GOAL;
			cout << "CASE: Only right" << endl;
		} else if(foundL) {
			//We have only left line
			//offset with one and a half the size of road to the right
			float a = tan(supLeftLine.slope * M_PI / 180);
			float b = supLeftLine.p1.y - supLeftLine.p1.x * a;
			int leftGoalX = (h - b)/ a;
			cout << "Left line X: " << leftGoalX << endl;
			//cout << a << "*x + " << b << endl;
			calcRoadAngle = getRoadAngle(3, supLeftLine.slope);
			calcRoadSize = getRoadSize(calcRoadAngle);
			int expectedDashLineX = leftGoalX + calcRoadSize;
			float expectedDashLineAngle = 180 - abs(supLeftLine.slope) - calcRoadAngle;
			if(expectedDashLineAngle > 90) {
				expectedDashLineAngle = expectedDashLineAngle - 180;
			}			
			cout << "Road angle prediction: " << calcRoadAngle << endl;
			cout << "Road size prediction: " << calcRoadSize << endl;
			cout << "Expected dash line X: " << expectedDashLineX << endl;
			cout << "Expected dash line angle: " << expectedDashLineAngle << endl;
			float da = tan(expectedDashLineAngle * M_PI / 180);
			float db = goalP.y - expectedDashLineX * da;
			//cout << da << "*x + " << db << endl;
			if (da != a) {
				vp.x = (b - db) / (da - a);
			}
			vp.y = a*vp.x + b;
			foundGoal = true;
			goalP.x = expectedDashLineX + calcRoadSize*ROAD_GOAL;
			cout << "CASE: Only left" << endl;
		}
	}
	m_lines = new Lines(leftLine, dashLine, rightLine);
	//If we have a goal set the position and
	cout << "Road size diff: " << abs(currRoadSize - calcRoadSize) << endl;
	if(foundGoal && abs(currRoadSize - calcRoadSize) < 0.5*currRoadSize) {
		//Suspect this position as the car position
		Point position;
		position.x = w/2;
		position.y = h;
		Point heading;
		heading.x = w/2;
		heading.y = 0;
		//Create car orientation vector
		CustomLine current;
		current.p1 = heading;
		current.p2 = position;
		current.slope = getLineSlope(heading, position);
        	m_lines->setCurrentLine(current);
		//Set your goal
		CustomLine goal;
		goal.p1 = vp;
		goal.p2 = goalP;
		goal.slope = getLineSlope(vp, goalP); 
		/*if(goal.slope > 90) {
			goal.p1.x = goal.p1.x + 60;
			goal.p2.x = goal.p2.x + 60;
		} else {
			goal.p1.x = goal.p1.x - 60;
			goal.p2.x = goal.p2.x - 60;
		}*/
		m_lines->setGoalLine(goal);
	} else {
		cout << "CASE: NONE" << endl;
	}
	//mylog.close();
    }
    return *m_lines;
}

CustomLine LineDetector::createLineFromRect(RotatedRect* rect, int sizeX, int sizeY) {
    CustomLine l;
    Point pt1, pt2;
    //cout << "[centerx, centery] = [" << rect->center.x << "," << rect->center.y << "]" << endl;
    //cout << "Sizes: " << sizeX << " " << sizeY;
    if(rect->angle < 90 ) {
        float angle = rect->angle * M_PI / 180; 
        float xOffset = cos(angle) * sizeY / 2;
        float yOffset = sin(angle) * sizeY / 2;
        pt1.y = rect->center.y + yOffset;
        pt1.x = rect->center.x + xOffset;
        pt2.y = rect->center.y - yOffset;
        pt2.x = rect->center.x - xOffset; 
    } else {
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
    return l;
}

void LineDetector::findLines(cv::Mat &outputImg) {
    vector<vector<Point> > contours;
    vector<Vec4i> hierarchy;
    cntDash = 0;
    cntSolid = 0;
    /// Find contours
    findContours( m_frame, contours, hierarchy, CV_RETR_TREE, CV_CHAIN_APPROX_SIMPLE, Point(0, 0) );
    dashLines = vector<CustomLine> (contours.size());
    solidLines = vector<CustomLine> (contours.size());
    vector<vector<Point> > contours_poly( contours.size() );
    /// Approximate contours to polygons + get min area rects
    for( int i = 0; i < contours.size(); i++ )
    {
	approxPolyDP( Mat(contours[i]), contours_poly[i], 3, true );
	RotatedRect rect = minAreaRect(contours_poly[i]);
	Point2f rect_points[4]; rect.points( rect_points );
	//cout << "Angle: " << rect.angle << endl;
	int sizeX = 0, sizeY = 0, sizeR = 0;
	Point shortSideMiddle;
	Point longSideMiddle;	
	// Find rect sizes
	for( int j = 0; j < 4; j++ ) {
	  //cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y << "]" << endl;
	  sizeR = cv::sqrt(cv::pow((rect_points[j].x - rect_points[(j+1)%4].x), 2) 
				+ cv::pow((rect_points[j].y - rect_points[(j+1)%4].y), 2)); 
	  //cout << "Size:" << sizeR << endl;
	  if(sizeX == 0) {
		sizeX = sizeR;
		shortSideMiddle.x = (rect_points[j].x + rect_points[(j+1)%4].x)/2;
		shortSideMiddle.y = (rect_points[j].y + rect_points[(j+1)%4].y)/2;
	  } else if(sizeY == 0 && sizeR != sizeX) {
		sizeY = sizeR;
		longSideMiddle.x = (rect_points[j].x + rect_points[(j+1)%4].x)/2;
		longSideMiddle.y = (rect_points[j].y + rect_points[(j+1)%4].y)/2;
	  }
	}
	if(sizeX > sizeY) {
		Point2f temp;
		sizeR = sizeX;
		sizeX = sizeY;
		sizeY = sizeR;
		temp = longSideMiddle;
		longSideMiddle = shortSideMiddle;
		shortSideMiddle = temp;
	}

	//Find real rect angle
	Point rectCenter;
	rectCenter.x = rect.center.x;
	rectCenter.y = rect.center.y;
        rect.angle = getLineSlope(shortSideMiddle, rectCenter);
	//Print sizes
	/*if(sizeX > 0 && sizeY > 0) {
		cout << "Sizes [x,y] = [" << sizeX << "," << sizeY << "]" << endl;
		int boundLow = m_config.XTimesYMin*sizeX;
		int boundHigh = m_config.XTimesYMax*sizeX;
		int boundHigh2 = m_config.maxY;
		cout << "Bounds: " << boundLow << "," << boundHigh << "," << boundHigh2 << endl;
	}*/

	int area = sizeX * sizeY;
	/*if(area > 0) {
		cout << "Area" << area << endl;
	}*/
	//Classify dash lines and solid lines
	if(sizeY > m_config.XTimesYMin*sizeX && sizeY < m_config.XTimesYMax*sizeX && sizeY < m_config.maxY) {
             dashLines[cntDash] = createLineFromRect(&rect, sizeX, sizeY);
             cntDash++;
             /*CustomLine l = createLineFromRect(&rect, sizeX, sizeY);
	     float a = tan(M_PI * l.slope / 180);
	     float b = l.p1.y - l.p1.x * a;
	     float positionX = l.p1.x;
	     if(abs(a) > 0.001) {
		positionX = (h - b) / a;
	     }
	     if(l.slope < 0 && positionX > 550) {
		solidLines[cntSolid] = l;
	     	cntSolid++;
	     } else {
	     	dashLines[cntDash] = createLineFromRect(&rect, sizeX, sizeY);
	     	cntDash++;
	     }*/
	} else if(sizeY > sizeX && sizeY > (m_config.maxY/2) && area < m_config.maxArea * 10000){
	     solidLines[cntSolid] = createLineFromRect(&rect, sizeX, sizeY);
	     cntSolid++;
	}
    }

    //Filter dashes outside the solid lines and merge solid lines
    for(int j=0; j < cntSolid; j++) {
	float a = tan(M_PI * solidLines[j].slope / 180);
	Point center;
	center.x = (solidLines[j].p1.x + solidLines[j].p2.x) / 2;
	center.y = (solidLines[j].p1.y + solidLines[j].p2.y) / 2;
	float b = center.y - center.x * a;
	//cout << "Equation [a,b]: [" << a << "," << b << "]" << endl;
	//cout << "Dashes" << endl;
	if((solidLines[j].slope > MIN_ANGLE - 5 && max(solidLines[j].p1.x, solidLines[j].p1.x) > w/2) ||
		(solidLines[j].slope < (-1) * (MIN_ANGLE - 5) && min(solidLines[j].p1.x, solidLines[j].p1.x) < w/2)) {
		for(int l=0; l < cntDash; l++) {
	 	    Point dashCenter;
		    dashCenter.x = (dashLines[l].p1.x + dashLines[l].p2.x) / 2;
		    dashCenter.y = (dashLines[l].p1.y + dashLines[l].p2.y) / 2;
		    float res = a*dashCenter.x + b;
		    //cout << "[res, y] = [" << res << "," << dashCenter.y << "]" << endl;
		    //cout << "[x, y] = [" << dashCenter.x << "," << dashCenter.y << "]" << endl;
		    if(res > dashCenter.y) {
			dashLines[l] = dashLines[cntDash-1];
			cntDash--;
			l--;
			//cout<< cntDash <<endl;
		    }
		}
		//cout << "Solids" << endl;
		for(int k=j+1; k < cntSolid; k++) {
		    Point sldCenter;
		    sldCenter.x = (solidLines[k].p1.x + solidLines[k].p2.x) / 2;
		    sldCenter.y = (solidLines[k].p1.y + solidLines[k].p2.y) / 2;
		    float res = a*sldCenter.x + b;
		    if(res > sldCenter.y) {
			solidLines[k] = solidLines[cntSolid-1];
			cntSolid--;
			k--;
			//cout<< cntSolid <<endl;
		    }
		}
	}
    }

    //Filter lines with very small angles 
    //Solid  
    for(int i=0; i < cntSolid; i++)
    {
	CustomLine l = solidLines[i]; 
	int minAngle = MIN_ANGLE - 5;
        //cout << "Slope: " << l.slope << " min is " << minAngle << endl;
        if(abs(l.slope) < minAngle )
        {
            solidLines[i] = solidLines[cntSolid-1];
	    cntSolid--;
	    i--;
	    foundStopStartLine = true;
        }
    }
    //Dash also positioned too high on the image or too left or too right
    for(int i=0; i < cntDash; i++)
    {
	CustomLine l = dashLines[i];
	int dashCenterX = (l.p1.x + l.p2.x) / 2;
	int dashCenterY = (l.p1.y + l.p2.y) / 2;
	//cout << "Slope: " << l.slope << " min is " << MIN_ANGLE << endl;
        if((l.slope < MIN_ANGLE && l.slope > ((-1) * MIN_ANGLE)) || (dashCenterY < h/15) || (dashCenterX > 19*w/20)) //|| (dashCenterX < w/20) too left //too high
        {
            dashLines[i] = dashLines[cntDash-1];
	    cntDash--;
	    i--;
        }
    }

    cout << "Dashes: " << cntDash << endl;
    cout << "Solids: " << cntSolid << endl;
    cout << "StopLine: " << foundStopStartLine << endl;

    //Print lines
    for(int i = 0; i < cntDash; i++) {
	line(outputImg, dashLines[i].p1, dashLines[i].p2, 45, 2);
	cout << "Dash line angle: " << dashLines[i].slope << endl;
    }
    for(int i = 0; i < cntSolid; i++) {
	line(outputImg, solidLines[i].p1, solidLines[i].p2, 0, 2);
	cout << "Solid line angle: " << solidLines[i].slope << endl;
    }
}

int LineDetector::detectHorizontalLine(Mat canny_roi, int dist)
{
    vector<Vec4i> lines;
    // Hough line detection
    HoughLinesP(canny_roi, lines, 1, CV_PI/180, 50, 100, 100);
    vector<Vec4i> likely_lines;
    for (vector<Vec4i>::iterator it = lines.begin(); it != lines.end(); it++)
    {
        int xA = (*it)[0], yA = (*it)[1];
        int xB = (*it)[2], yB = (*it)[3];
        double theta = atan2(yB-yA, xB-xA);
        //cout << "Angle: " << theta*180/CV_PI << endl;
        if (theta >= -CV_PI/36 && theta <= CV_PI/36)
        {
            //&&_roi.cols/2 && xB >= src_roi.cols/2) {
            likely_lines.push_back(*it);
            //cout << "(" << xA << ", " << yA << "), (" << xB << ", " << yB << ")" << endl;
        }
    }
    int yMax = 0;
    vector<Vec4i>::iterator ptr1, ptr2;
    for (vector<Vec4i>::iterator it1 = likely_lines.begin(); it1 != likely_lines.end(); it1++)
    {
        for (vector<Vec4i>::iterator it2 = it1+1; it2 != likely_lines.end(); it2++)
        {
            Point p1A = Point((*it1)[0], (*it1)[1]);
            Point p1B = Point((*it1)[2], (*it1)[3]);
            Point p2A = Point((*it2)[0], (*it2)[1]);
            Point p2B = Point((*it2)[2], (*it2)[3]);
            int y1Avg = (p1A.y+p1B.y)/2;
            int y2Avg = (p2A.y+p2B.y)/2;
            if (abs(y1Avg-y2Avg) <= dist)
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
        return canny_roi.rows-yMax;
    }
    else
    {
        return -1;
    }
}

float LineDetector::getDist(const Point p1, const Point p2) const {
    return sqrt(pow(p1.x-p2.x, 2) + pow(p1.y-p2.y, 2));
}

int LineDetector::detectStartLine(int dist)
{
    Rect roi_left, roi_right;
    roi_left = Rect(0, m_frameCanny.rows/2, m_frameCanny.cols/2, m_frameCanny.rows/2);
    roi_right = Rect(m_frameCanny.cols/2, m_frameCanny.rows/2, m_frameCanny.cols/2, m_frameCanny.rows/2);
    int yLineLeft = detectHorizontalLine(m_frameCanny(roi_left), dist);
    int yLineRight = detectHorizontalLine(m_frameCanny(roi_right), dist);
    if (abs(yLineLeft-yLineRight) <= 10)
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
    Rect roi = Rect(m_frameCanny.cols/2, m_frameCanny.rows/2, m_frameCanny.cols/2, m_frameCanny.rows/2);
    src_roi = m_frameCanny(roi);
    return detectHorizontalLine(src_roi, dist);
}

/** Get the slope of a line defined by two points */
float LineDetector::getLineSlope(Point &p1, Point &p2)
{
    float slope = M_PI/2;
    if((p1.x - p2.x)!= 0)
    {
        slope = (p1.y - p2.y) / ((float)(p1.x - p2.x));
        slope = atan(slope);
    }
    if(slope < 0)
    {
        return 180 + (slope*180/M_PI);
    }
    return slope*180/M_PI;
}

/** Predicts the road angle considering one detected line */
int LineDetector::getRoadAngle(int lineDetected, int lineAngle){
   int roadAngle = ROAD_ANGLE;
   float c1 = (roadAngle - 29.1)/(180 - abs(MID_DASH_ANGLE) - roadAngle);
   float c2 = (roadAngle - 65.0)/(MID_DASH_ANGLE + 90);
   cout << "Road angle consts: " << c1 << "," << c2 << endl;
   switch(lineDetected) {
       case 1: {
           //founded line is right line
	   if(lineAngle < 63 && lineAngle >= 25) {
		roadAngle = 29.1 + c1*lineAngle;//1.44
	   } else if(lineAngle < 25) {
		roadAngle = 65;
	   }
       }; break;
       case 2: {
           //founded line is dash line
	   if(lineAngle < 0) {
               roadAngle = 65 + (lineAngle + 90) * c2;//0.59;
           } else {
               roadAngle = 65 + (lineAngle - 90) * c2;//0.59;
           }
       }; break;
       case 3: {
           //founded line is left line
	   if(lineAngle > -63 && lineAngle <= -25) {
		roadAngle = 29.1 - c1*lineAngle; //1.44
	   } else if(lineAngle > -25) {
		roadAngle = 65;
	   }
       }; break;
   }
   return roadAngle;
}

/** Predicts the road size considering the roadAngle */
int LineDetector::getRoadSize(int roadAngle) {
   int roadSize = ROAD_SIZE;

   if(roadAngle > ROAD_ANGLE && roadAngle < (ROAD_ANGLE + 15)) {
	roadSize = 5*roadAngle + (ROAD_SIZE - ROAD_ANGLE*5);
   } else if(roadAngle > (ROAD_ANGLE + 15)) {
	float a = (ROAD_SIZE -  5)/5;
	float b = 3*ROAD_SIZE - (ROAD_ANGLE + 25) * a;
	roadSize = roadAngle * a + b;
   } else if(roadAngle > (ROAD_ANGLE - 15) && roadAngle < ROAD_ANGLE) {
	roadSize = 5*(2*ROAD_ANGLE - roadAngle) + (ROAD_SIZE - ROAD_ANGLE*5);
   } else if(roadAngle < (ROAD_ANGLE - 15) && roadAngle > (ROAD_ANGLE - 25)) {
	cout << "SZ S" << endl;
	float a = (ROAD_SIZE -  5)/5;
	float b = 3*ROAD_SIZE - (ROAD_ANGLE + 25) * a;
	roadSize = (2*ROAD_ANGLE - roadAngle) * a + b;
   }

   return roadSize;
}

Point2f LineDetector::getWorldPoint(Point2i p){
    p.x = p.x - 376;
    p.y = 480 - p.y;

    CameraStruct cam;
    cam.focal = 420;
    cam.focal2 = 420;
    cam.u0 = 752/2;
    cam.v0 = 240;
    cam.height = 192;
    cam.length = 88;
    cam.theta0 = 24;
    cam.beta0 = 87;
    cam.gamma0 = 90;
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

   double u, v, w;
   Point2f res;
   w = 1 / (camera_matrix.at<double>(2,0) * p.x + camera_matrix.at<double>(2,1)* p.y
       + camera_matrix.at<double>(2,2));
   u = w * p.x;
   v = w * p.y;

   res.x = camera_matrix.at<double>(0,0) * u + camera_matrix.at<double>(0,1) * v
    + camera_matrix.at<double>(0,2) * w;
   res.y = camera_matrix.at<double>(1,0) * u + camera_matrix.at<double>(1,1) * v
    + camera_matrix.at<double>(1,2) * w;
   
   return res;
}

int LineDetector::getIntersectionWithBottom(CustomLine l) const {
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if(abs(a) > 0.001) {
        positionX = (h - b) / a;
    }
    return positionX;
}


}

#include "LineDetector.h"
#include <stdio.h>
#include <math.h>

#include "opencv2/core/core.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

namespace carolocup
{

using namespace std;
using namespace cv;

int cntDash = 0;
int cntSolid = 0;
vector<CustomLine> dashLines;
vector<CustomLine> solidLines;
int h, w;

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
    /// Detect edges using Threshold
    threshold( m_frame, m_frame, cfg.th1, cfg.th2, cfg.hlTh );
    //cv::Canny(getFirst, getFirst, cfg.caThVal, cfg.caThMax, cfg.caThTyp);

    //Find dash and solid lines
    findLines(outputImg);


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
	Vec4i dashLine(0,0,0,0), leftLine(0,0,0,0), rightLine(0,0,0,0);

	bool foundR=false, foundL=false, foundD=false;
	CustomLine supDashLine, supRightLine, supLeftLine;
        //Pick the suitable dashLine
	cout << "Pick lines!" << endl;
	if(cntDash > 0) {
		for(int i = 0; i < cntDash; i++) {
		    if(dashLines[i].slope < supDashLine.slope) {
			supDashLine = dashLines[i];
		    }
		}
		cout << "Dash line slope: " << supDashLine.slope << endl;
		dashLine = Vec4i(supDashLine.p1.x, supDashLine.p1.y, supDashLine.p2.x, supDashLine.p2.y);
		foundD = true;
	}
	if(cntSolid > 0) {
		for(int i = 0; i < cntSolid; i++) {
		    int centerSolidLineX = (solidLines[i].p1.x + solidLines[i].p2.x)/2;
		    if(solidLines[i].slope < 90 && solidLines[i].slope > supRightLine.slope && w/2 < centerSolidLineX) {
			supRightLine = solidLines[i];
			foundR = true;
		    }
		}
		if(foundR) {
			cout << "Right line slope: " << supRightLine.slope << endl;
			rightLine = Vec4i(supRightLine.p1.x, supRightLine.p1.y, supRightLine.p2.x, supRightLine.p2.y);
		}
		for(int i = 0; i < cntSolid; i++) {
		    int centerSolidLineX = (solidLines[i].p1.x + solidLines[i].p2.x)/2;
		    if(solidLines[i].slope > 90 && solidLines[i].slope < supLeftLine.slope && w/2 > centerSolidLineX) {
			supLeftLine = solidLines[i];
			foundL = true;
		    }
		}
		if(foundL) {
			cout << "Left line slope: " << supLeftLine.slope << endl;
			leftLine = Vec4i(supLeftLine.p1.x, supLeftLine.p1.y, supLeftLine.p2.x, supLeftLine.p2.y);
		}
	} 
	Point vp;
	Point goalP;
	//Set goal height
	goalP.y = h;
	bool foundGoal;
	//Trace different image scenarious
	if(foundD) {
		//We have a dash line
		float da = (supDashLine.slope - 180) * M_PI / 180;
		float db = supDashLine.p1.y - supDashLine.p1.x * da;
		int dashGoalX = (goalP.y - db)/da;
		if(foundR) {
			//We have dash and right line
			//Calculate vanishing point
			float a = supRightLine.slope * M_PI / 180;
			float b = supRightLine.p1.y - supRightLine.p1.x * a;
			int rightGoalX = (goalP.y - b)/a;
			if (da != a) {
				vp.x = (b - db) / (da - a);
			}
			vp.y = da*vp.x + db;
			goalP.x = (dashGoalX + rightGoalX)/2;
			cout << "CASE: Dash and right" << endl;
		} else if(foundL){
			//We have dash and left line
			float a = supLeftLine.slope * M_PI / 180;
			float b = supLeftLine.p1.y - supLeftLine.p1.x * a;
			if (da != a) {
				vp.x = (b - db) / (da - a);
			}
			vp.y = da*vp.x + db;
			goalP.x = dashGoalX + ROAD_SIZE/2;
			cout << "CASE: Dash and left" << endl; 
		} else {
			//We have only dash line
			//offset with half the size of road to the right
			int dashHeightCrossingX = (h - db)/ da;
			vp.x = dashHeightCrossingX + ROAD_SIZE/2;
			vp.y = da*vp.x + db;
			goalP.x = dashGoalX + ROAD_SIZE/2;
			cout << "CASE: Only dash" << endl;
		}
		foundGoal = true;
	} else {
		//No dash line
		if(foundR) {
			//We have only right line
			//offset with half the size of road to the left
			float a = supRightLine.slope * M_PI / 180;
			float b = supRightLine.p1.y - supRightLine.p1.x * a;
			int dashHeightCrossingX = (h - b)/ a;
			vp.x = dashHeightCrossingX - ROAD_SIZE/2;
			vp.y = a*vp.x + b;
			foundGoal = true;
			goalP.x = vp.x;
			cout << "CASE: Only right" << endl;
		} else if(foundL) {
			//We have only left line
			//offset with one and a half the size of road to the right
			float a = supLeftLine.slope * M_PI / 180;
			float b = supLeftLine.p1.y - supLeftLine.p1.x * a;
			int dashHeightCrossingX = (h - b)/ a;
			vp.x = dashHeightCrossingX + 3*ROAD_SIZE/2;
			vp.y = a*vp.x + b;
			foundGoal = true;
			goalP.x = vp.x;
			cout << "CASE: Only left" << endl;
		}
	}
	m_lines = new Lines(leftLine, dashLine, rightLine);
	//If we have a goal set the position and 
	if(foundGoal) {
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
		m_lines->setGoalLine(goal);
	} else {
		cout << "CASE: NONE" << endl;
	}
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
	//cout << "Sizes [x,y] = [" << sizeX << "," << sizeY << "]" << endl;

	//Classify dash lines and solid lines
	if(sizeY > m_config.XTimesYMin*sizeX && sizeY < m_config.XTimesYMax*sizeX && sizeY < m_config.maxY ) {
	     dashLines[cntDash] = createLineFromRect(&rect, sizeX, sizeY);;
	     cntDash++;
	} else if(sizeY > 4*m_config.XTimesYMin*sizeX && sizeY > (m_config.maxY/2)){
	     solidLines[cntSolid] = createLineFromRect(&rect, sizeX, sizeY);
	     cntSolid++;
	}
    }

    //Filter dashes outside the solid lines and merge solid lines
    for(int j=0; j < cntSolid; j++) {
	float a = M_PI * solidLines[j].slope / 180;
	Point center;
	center.x = (solidLines[j].p1.x + solidLines[j].p2.x) / 2;
	center.y = (solidLines[j].p1.y + solidLines[j].p2.y) / 2;
	float b = center.y - center.x * a;
	//cout << "Equation [a,b]: [" << a << "," << b << "]" << endl;
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
	for(int k=j+1; k < cntSolid; k++) {
	    Point sldCenter;
	    sldCenter.x = (solidLines[k].p1.x + solidLines[k].p2.x) / 2;
	    sldCenter.y = (solidLines[k].p1.y + solidLines[k].p2.y) / 2;
	    float res = a*sldCenter.x + b;
	    if(res > sldCenter.y) {
		solidLines[k] = solidLines[cntSolid-1];
		cntSolid--;
		k--;
		//cout<< cntDash <<endl;
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
        if(l.slope < minAngle && l.slope > ((-1) * minAngle) )
        {
            solidLines[i] = solidLines[cntSolid-1];
	    cntSolid--;
	    i--;
        }
    }
    //Dash and also positioned too high on the image or too left or too right
    for(int i=0; i < cntDash; i++)
    {
	CustomLine l = dashLines[i];
	int dashCenterX = (l.p1.x + l.p2.x) / 2;
	int dashCenterY = (l.p1.y + l.p2.y) / 2;
	//cout << "Slope: " << l.slope << " min is " << MIN_ANGLE << endl;
        if((l.slope < MIN_ANGLE && l.slope > ((-1) * MIN_ANGLE)) || (dashCenterY < h/10) || (dashCenterX < h/10))
        {
            dashLines[i] = dashLines[cntDash-1];
	    cntDash--;
	    i--;
        }
    }

    cout << "Dashes: " << cntDash << endl;
    cout << "Solids: " << cntSolid << endl;

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
        return 180 + (slope/M_PI)*180;
    }
    return (slope/M_PI)*180;
}

/** Get distance between two points */
float LineDetector::getDist(const Point p1, const Point p2) const
{
    return sqrt(pow(p1.x-p2.x, 2) + pow(p1.y-p2.y, 2));
}

}

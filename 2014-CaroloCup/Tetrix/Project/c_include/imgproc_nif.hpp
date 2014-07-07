#ifndef IMGPROC_NIF_HPP
#define IMGPROC_NIF_HPP

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv/highgui.h>
#include <opencv/cv.h>

#include <iostream>
#include <sstream>
#include <sys/time.h>
#include <stdio.h>
#include <math.h>

using namespace cv;
using namespace std;

//#################################### DEFINES #####################################
#define LINECOVERAGE 250.0

#define CENTER_X 376
#define CENTER_Y 240

#define LEFT_LINE 3
#define DASHED_LINE 2
#define RIGHT_LINE 1

#define LEFT_MIN 327.0
#define LEFT_MAX 359.0
#define DASHED_MIN 250.0
#define DASHED_MAX 320.0
#define RIGHT_MIN 210.0
#define RIGHT_MAX 238.0

#define PI 3.141592653589793238462

//#################################### STRUCTS #####################################
typedef struct _range {
  int left;
  int mid;
  int right;
  int length;
} range;

typedef struct _frame_t {
  IplImage* _frame;
} frame_t;

//############################## FUNCTION DEFINITIONS ##############################
float dist(CvPoint p1, CvPoint p2);
float dist(Point2f p1, Point2f p2);
bool is_trash(vector<vector<Point2i> > line);
float get_angle( Point2f a, Point2f b, Point2f c );
Point2f center_point(Point2f a, Point2f b);
int find_dashed(vector<vector<vector<vector<Point2i> > > > grouped);

//############################## FUNCTION DECLARATIONS #############################
float dist(CvPoint p1, CvPoint p2) {
  return sqrt((p2.y - p1.y) * (p2.y - p1.y) + (p2.x - p1.x) * (p2.x - p1.x));
}

float dist(Point2f p1, Point2f p2) {
  return sqrt((p2.y - p1.y) * (p2.y - p1.y) + (p2.x - p1.x) * (p2.x - p1.x));
}

bool is_trash(vector<vector<Point2i> > line){

  if(line.size() < 5)
    return true;

  for (unsigned int i = 0; i < line.size(); ++i) {
    float length = (((line[i][1].y  - LINECOVERAGE) / (480.0 - LINECOVERAGE)) * 30.0) + 10;
    unsigned int width = line[i][1].x - line[i][0].x;
    if(width > length ){
      //cout << "WIDTH : " << width << " LENGTH "<< length<<  " HOW MUCH LEFT? "<< line.size() - i << "  I  "<<  i <<endl;
      if(line.size() > i + 5 ){
	//cout << "REMOVED" << endl;
	return true;
      }
    }
    if(width > line.size())
      return true;
  }

  return false;
}

float get_angle( Point2f a, Point2f b, Point2f c ){
  Point2f ab = Point2f(b.x - a.x, b.y - a.y );
  Point2f cb = Point2f(b.x - c.x, b.y - c.y );

  float dot = (ab.x * cb.x + ab.y * cb.y); // dot product
  float cross = (ab.x * cb.y - ab.y * cb.x); // cross product

  float alpha = atan2(cross, dot);

  return alpha * 180 / PI;
}


Point2f center_point(Point2f a, Point2f b){
  return Point2f((a.x+b.x)/2 , (a.y+b.y)/2 );
}

int find_dashed(vector<vector<vector<vector<Point2i> > > > grouped){
  for (unsigned int i = 0; i < grouped.size(); i++) {
    if(grouped[i].size() > 1){
      bool correct = true;
      for (unsigned int j = 0; j < grouped[i].size() - 1; j++) {
	int last = grouped[i][j].size() -1;
	Point2f start = center_point(grouped[i][j][last][0] , grouped[i][j][last][1]);

	Point2f end  = center_point(grouped[i][j+1][0][0] , grouped[i][j+1][0][1]);
	float distance =  start.y - end.y;// dist(start, end);
	float length = (start.y - 250) / 160 * 65 + 8;
	if(distance > length){
	  correct = false;
	}
      }
      if(correct){
	return i;
      }
    }
  }
  return -1;
}

#endif /* IMGPROC_NIF_HPP */

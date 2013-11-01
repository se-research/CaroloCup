#include <iostream>
#include <sstream>
#include <sys/time.h>
#include <stdio.h>
#include <math.h>

#include "camera_functions.hpp"
#include "ueye.h"
 
#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv/highgui.h>
#include <opencv/cv.h>

using namespace cv;
using namespace std;

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

typedef struct _range {
  int left;
  int mid;
  int right;
  int length;
} range;

void draw_line(IplImage* src, vector<vector<Point2i > >line, CvScalar color) {
  for (unsigned int i = 0; i < line.size(); i++) {
    cvLine(src, cvPoint(line[i][0].x, line[i][0].y),
	   cvPoint(line[i][1].x, line[i][1].y), color, 4, 8, 0);
  }
}

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

int main() {
  /*********************** remove when not using video ***********************/
  //CvCapture* capture = cvCreateFileCapture("/home/robin/Downloads/crap.mov");

   init_camera(); 
  while(1) {
    

    
    //IplImage* src;
    //src = cvQueryFrame(capture);
    char* imgPointer;
    bool img_retrieved = get_image(imgPointer);
    IplImage* gray = cvCreateImage(cvSize(752,480), IPL_DEPTH_8U, 1);
    gray -> imageData = imgPointer;
 
  
    if(!gray) break;
    /*********************** remove when not using video ***********************/

    IplImage* src = cvCreateImage(cvGetSize(gray), IPL_DEPTH_8U, 3);
    cvCvtColor(gray, src, CV_GRAY2RGB);

    CvScalar colors[4];
    colors[0] = CV_RGB(0,0,0);
    colors[1] = CV_RGB(255,0,0);
    colors[2] = CV_RGB(0,255,0);
    colors[3] = CV_RGB(0,0,255);
    int color = 0;


    int row = 423, column;

    int current_row;

    range line_range;
    line_range.length = 25;

    vector<vector<vector<Point2i> > > lanes;

   
    while (row > LINECOVERAGE) {
      bool bottom_found;
      color++;
      bottom_found = false;
      vector<vector<Point2i> > current_line;
      while (!bottom_found) {
	for (column = 151; column < 600; column++) {
	  if ((uint) (gray->imageData[row * 752 + column]) > 100) {
	    gray->imageData[row * 752 + column] = (char) 0;
	    current_row = row;
	    bottom_found = true;
	    line_range.mid = column;
	    break;
	  }
	}
	row -= 5;
      }
      
      int valid_row = 3;
      while (valid_row > 0) {
	valid_row = false;
	int on_white = 0;
	bool hit_white = false;

	line_range.length = (((current_row - LINECOVERAGE) / (480.0 - LINECOVERAGE)) * 30.0) + 10;

	line_range.left = MAX(151,line_range.mid - line_range.length);
	line_range.right = MIN(600,line_range.mid + line_range.length);

	column = line_range.left;

	vector<Point2i > single_line;

	while ( (!hit_white && column < line_range.right) || (on_white > 0 && column < 600)) {
	  if ((uint) (gray->imageData[current_row * 752 + column]) > 100) {
	    gray->imageData[current_row * 752 + column] = (char) 0;
	    single_line.push_back(Point2i(column, current_row));
	    hit_white = true;
	    on_white = 3;
	    valid_row = 3;
	  } else {
	    on_white--;
	  }
	  column++;
	}
	if (valid_row ==  3) {

	  CvPoint center;
	  center.x = (single_line[0].x + single_line[single_line.size() - 1].x) / 2;
	  center.y = current_row;

	  vector<Point2i> left_right_points;
	  left_right_points.push_back(Point2i(single_line[0].x ,current_row));
	  left_right_points.push_back(Point2i(single_line[single_line.size() - 1].x ,current_row));

	  current_line.push_back(left_right_points);
	  line_range.mid = center.x;
	} else {
	  valid_row--;
	}
	current_row--;
      }
      if (current_line.size() > 1 )
	lanes.push_back(current_line);

    } // end of frame while


    unsigned int lanes_size = lanes.size();
    for (unsigned int i = 0; i < lanes.size(); i++) {
      if(lanes[i].size() > 11){
	for (unsigned int j = 5; j < lanes[i].size()-6; j++) {
	  Point2f start;
	  start.x = (lanes[i][j-5][0].x + lanes[i][j-5][1].x) / 2;
	  start.y = lanes[i][j-5][0].y;

	  Point2f end;
	  end.x = (lanes[i][j+5][0].x + lanes[i][j+5][1].x) / 2;
	  end.y = lanes[i][j+5][0].y;

	  Point2f center;
	  center.x = (lanes[i][j][0].x + lanes[i][j][1].x) / 2;
	  center.y = lanes[i][j][0].y;

	  float alpha = get_angle(end,center,start);
	  if((alpha > 80) || ( alpha < -40) ){
	  }else{
	    vector<vector<Point2i> > temp;
	    int c= 0;
	    for (unsigned int k = j; k < lanes[i].size(); k++) {
	      temp.push_back(lanes[i][k]);
	      c++;
	    }
	    lanes.push_back(temp);
	    for (int k = 0; k < c; k++) {
	      lanes[i].pop_back();
	    }
	    j = lanes[i].size();
	    break;
	  }
	}
      }
    }

    vector<vector<vector<vector<Point2i> > > > grouped;

    vector<vector<vector<Point2i> > > temp;
    temp.push_back(lanes[0]);
    grouped.push_back(temp);

    for (unsigned int i = 1; i < lanes_size; i++) {
      bool connected = false;
      Point2f lanes_center = Point2f((lanes[i][0][0].x  + lanes[i][0][1].x)/2 , lanes[i][0][0].y);
      for (unsigned int j = 0; j < grouped.size(); j++) {
	Point2f grouped_point_left = grouped[j][grouped[j].size()-1][grouped[j][grouped[j].size()-1].size()-1][0];
	Point2f grouped_point_right = grouped[j][grouped[j].size()-1][grouped[j][grouped[j].size()-1].size()-1][1];
	Point2f grouped_point_center = Point2f((grouped_point_left.x + grouped_point_right.x)/2 , grouped_point_left.y);
	float distance_group = dist(grouped_point_center , lanes_center);

	if(grouped_point_center.y >= lanes_center.y && distance_group < 100){

	  int first  = div(grouped[j][grouped[j].size()-1].size() , 4).quot;
	  int last = grouped[j][grouped[j].size()-1].size() - first -1;
	  Point2f start = center_point(grouped[j][grouped[j].size()-1][first][0], grouped[j][grouped[j].size()-1][first][1]);
	  Point2f end = center_point(grouped[j][grouped[j].size()-1][last][0], grouped[j][grouped[j].size()-1][last][1]);
	  int x = (((start.x - end.x) * (lanes_center.y - start.y)) / (start.y - end.y)) + start.x;

	  float distance = dist(lanes_center , Point2f(x,lanes_center.y));
	  if(distance < 25){
	    if(!is_trash(lanes[i])){
	      grouped[j].push_back(lanes[i]);
	    }
	    connected = true;
	  }
	}//end of if(grouped_point_center... )
	

      }//end of for(unsigned int j... )
      if(!connected){
	if(!is_trash(lanes[i])){
	  temp.clear();
	  temp.push_back(lanes[i]);
	  grouped.push_back(temp);
	}
      }
    }//end of for(unsigned int i... )


    int dash_index = find_dashed(grouped);

 
    vector<Point2f> final_result;
    if(dash_index != -1){
      for (unsigned int j = 0; j < grouped[dash_index].size(); j++) 
	{
	  draw_line(src, grouped[dash_index][j], colors[1]);
	}

      for(unsigned int i = 0; i< grouped[dash_index].size() ; i++)
	{
	  Point2f sum = Point2f(0,0); 
	  int count = 0;
	  int first_index = (int) (grouped[dash_index][i].size() / 10);
	  int last_index = (int) ((grouped[dash_index][i].size() / 10) * 9);
      

	  Point2f first =  center_point(grouped[dash_index][i][first_index][0], 
					grouped[dash_index][i][first_index][1]);
	  Point2f second = center_point(grouped[dash_index][i][last_index][0], 
					grouped[dash_index][i][last_index][1]);

	  cvLine(src, cvPoint(first.x, first.y),
		 cvPoint(first.x, first.y), colors[2], 4, 8, 0);      

	  cvLine(src, cvPoint(second.x, second.y),
		 cvPoint(second.x, second.y), colors[2], 4, 8, 0);      
	  
	  final_result.push_back(first);
	  final_result.push_back(second);
	}

    }

    

    cvShowImage("SRCFINAL", src);
    cvWaitKey(30);
  }//end of while(1)

  /************************ remove when not using video ************************/  
  return 0;

  
}/**** END OF MAIN ****/

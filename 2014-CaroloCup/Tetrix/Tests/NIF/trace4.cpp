#include <iostream>
#include <sstream>
#include <time.h>
#include <stdio.h>
#include <math.h>

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

#define PI 3.141592653589793238462

typedef struct _range {
  int left;
  int mid;
  int right;
  int length;
} range;

int check_region(int x1, int y1, int x2, int y2, int radius);

int main() {

  IplImage* src = cvLoadImage("/home/robin/Downloads/pic.png");
  //	IplImage* src = cvLoadImage("/home/khashayar/undispic.png");
  //	Mat src = imread("/home/khashayar/Pictures/1.png");
  //	Mat src = imread("/home/khashayar/Downloads/pic.png");

  IplImage* gray = cvCreateImage(cvGetSize(src), IPL_DEPTH_8U, 1);
  cvCvtColor(src, gray, CV_RGB2GRAY);

  //	cvShowImage("GRAY", gray);

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

  vector<vector<Point2i> > lanes;

  bool bottom_found;
  while (row > LINECOVERAGE){
    color++;
bottom_found = false;
    vector<Point2i> current_line;
    while (!bottom_found) {
      for (column = 151; column < 600; column++) {
	if ((uint) (gray->imageData[row * 752 + column]) > 100) {
	  //					current_line.push_back(Point2i(column, row));
	  gray->imageData[row * 752 + column] = (char) 0;
	  cvLine(src, cvPoint(column, row), cvPoint(column, row),
		 colors[color % 3]);
	  current_row = row;
	  bottom_found = true;
	  line_range.mid = column;
	  break;
	}
      }
      row -= 5;
    }

    cout << "ROW : " << row << endl;
    //		cout << column << "," << row << endl;
    bool valid_row = true;
    while (current_row > LINECOVERAGE) {
      valid_row = false;
      int on_white = 0;
      bool hit_white = false;

      line_range.length = (((current_row-LINECOVERAGE) /(480.0-LINECOVERAGE)) * 25.0) + 5;

      line_range.left = MAX(151,line_range.mid - line_range.length);
      line_range.right = MIN(600,line_range.mid + line_range.length);
      column = line_range.left;

      vector<Point2i> single_line;

      while ((on_white > 0 || !hit_white) && column < line_range.right) {
	if ((uint) (gray->imageData[current_row * 752 + column]) > 100) {
	  gray->imageData[current_row * 752 + column] = (char)0;
	  single_line.push_back(Point2i(column, current_row));
	  // cvLine(src, cvPoint(column, current_row),
	  // 	 cvPoint(column, current_row),
	  //	 colors[color % 3]);
	  hit_white = true;
	  on_white = 3;
	  valid_row = true;
	} else {
	  on_white--;
	}
	column++;
      }
      if (valid_row){
	CvPoint center;
	center.x = (single_line[0].x + single_line[single_line.size() - 1].x) / 2;
	center.y = current_row;
	current_line.push_back(center);
	line_range.mid = center.x;
	//cvLine(src, center, center, colors[color+1 % 3]);
      }else{
	if(current_line.size() > 1 ){
	  CvPoint start = current_line[0];
	  CvPoint end = current_line[current_line.size()-1];
	  line_range.mid = ( ( (start.x - end.x) * ((current_row -1) - start.y) ) / (start.y - end.y) ) + start.x;
	}

      }
      current_row --;
    }
    if(current_line.size() > 1)
      lanes.push_back(current_line);
    
    //cvShowImage("SRCFINAL", src);
    //cvWaitKey();

  } // end of frame while


  for(int i = 0; i<lanes.size(); i++){
    int last = MIN(10, lanes[i].size()-1);
    color = check_region(lanes[i][0].x, lanes[i][0].y, lanes[i][last].x, lanes[i][last].y, CENTER_X); 
    for(int j = 0; j<lanes[i].size(); j++){
      cvLine(src, 
	     cvPoint(lanes[i][j].x, lanes[i][j].y),
	     cvPoint(lanes[i][j].x, lanes[i][j].y),
	     colors[color], 4, 8, 0);
    }//end of for i
  }//end of for j
  cout << "DONE";
  
  //	cvSaveImage("/home/khashayar/just_sayin.png", src);
  cvShowImage("SRCFINAL", src);
  cvWaitKey();
  return 0;
}


/*****************************************************************************
 * Function that checks if a line is valid by checking its angle
 * If the line is valid it checks if it is within the region of interest
 *
 *****************************************************************************/
int check_region(int x1, int y1, int x2, int y2, int radius){
  float dist;
  float angle;
  float x;

  x =(float) ( (x2/1.0 - x1/1.0)*(240 - y2/1.0) / (y2/1.0 - y1/1.0) ) + x2/1.0;
  dist =(float) sqrt( pow((CENTER_X-x1/1.0),2) + pow((CENTER_Y-y1/1.0),2) );
  angle =(float) atan2(CENTER_Y-y1/1.0, CENTER_X-x1/1.0) * 180.0 / PI;
  
  if(angle < 0.0){ angle += 360.0; } // convert from -180-180 to 0-360 degrees

  // cout << "X: " << x << endl;
  // cout << "Distance: " << dist << endl;
  // cout << "Angle: " << angle << endl;

  if(200.0 > x || x > 550.0 ){ return 0; } // check if line is pointing towards center

  if(206.0 < angle && angle < 232.0 && 200.0 < dist && dist < radius ){
    return 1; // RIGHT 
  }else if(287.0 < angle && angle < 319.0 && 200.0 < dist && dist < radius ){
    return 2; // CENTER
  }else if(327.0 < angle && angle < 350.0 && 200.0 < dist && dist < radius ){
    return 3; // LEFT
  }else{
    return 0; // NOT IN ANY REGION
  }
}




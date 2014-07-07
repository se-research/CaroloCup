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
Point2f hit_point(Point2i point1, Point2i point2, Point2i point3, Point2i point4 , IplImage* src , CvScalar color);
bool touch_circle(Point2i point1, Point2i point2, Point2f circle , float r);
void draw_line(IplImage* src, vector<Point> line , CvScalar color);

int main() {

  cout << "HAHA";
  /*********************** remove when not using video ***********************/
  
  CvCapture* capture = cvCreateFileCapture("/home/khashayar/Downloads/crap.mov");
  int counter = 0;
  while(1) 
    {
      IplImage* src;
      src = cvQueryFrame(capture);
      //      IplImage* temp_ipl = cvCreateImage(cvGetSize(src), IPL_DEPTH_8U, 1);
      
      if(!src) break;
      /*********************** remove when not using video ***********************/
      
      //  IplImage* src = cvLoadImage("/home/khashayar/Downloads/pic.png");
      //	IplImage* src = cvLoadImage("/home/khashayar/undispic.png");
      //	Mat src = imread("/home/khashayar/Pictures/1.png");
      //	Mat src = imread("/home/khashayar/Downloads/pic.png");
/*      
      IplImage* gray = cvCreateImage(cvGetSize(src), IPL_DEPTH_8U, 1);
      cvCvtColor(src, gray, CV_RGB2GRAY);

      //	cvShowImage("GRAY", gray);

      CvScalar colors[4];
      colors[0] = CV_RGB(0,0,0);
      colors[1] = CV_RGB(255,0,0);
      colors[2] = CV_RGB(0,255,0);
      colors[3] = CV_RGB(0,0,255);
      int color = 0;
 

      int row = 453, column;

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
	  for (column = 50; column < 600; column++) {
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

	  line_range.left = MAX(50,line_range.mid - line_range.length);
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
    

      } // end of frame while

      int colorn = 0;
      for(int i = 0; i<lanes.size(); i++){
	for(int j = i+1; j<lanes.size(); j++){
	  colorn++;
	  Point2f hitp = hit_point(lanes[i][lanes[i].size()-1] , lanes[i][0], lanes[j][lanes[j].size()-1] , lanes[j][0] , src, colors[colorn % 3+1]);
	  //		  draw_line(src,lanes[i],colors[colorn%3+1]);
	  //		  draw_line(src,lanes[j],colors[colorn%3+1]);
	  //			  cvCircle(src, cvPoint(hitp.x,hitp.y),4,colors[colorn % 3+1]);
	  //		  cvShowImage("SRCFINAL", src);
	  //		  cvWaitKey();
	  if(hitp.y  < 250 && hitp.y > 200){
	    for(int k = j+1; k<lanes.size(); k++){
	      if(touch_circle(lanes[k][lanes[k].size()-1] , lanes[k][0], hitp, 10)){
		draw_line(src,lanes[i],colors[1]);
		draw_line(src,lanes[j],colors[1]);
		draw_line(src,lanes[k],colors[1]);
	      }

	    }
	  }
	}

      }


      //  for(int i = 0; i<lanes.size(); i++){
      //    int last = MIN(10, lanes[i].size()-1);
      //    color = check_region(lanes[i][0].x, lanes[i][0].y, lanes[i][last].x, lanes[i][last].y, CENTER_X);
      //    draw_line(src, lanes[i] , colors[color]);
      //  }//end of for i
      cvShowImage("SRCFINAL", src);
      cvWaitKey(30);
*/
      cout << "DONE";
      /*********************** remove when not using video ***********************/
    }//end of while
  //cvReleaseCapture(&capture);
  //cvDestroyWindow("Example2");  
  return EXIT_SUCCESS;
  /*********************** remove when not using video ***********************/
  //	cvSaveImage("/home/khashayar/just_sayin.png", src);
  //cvShowImage("SRCFINAL", src);
  //cvWaitKey();
  //return 0;
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


Point2f hit_point(Point2i p1, Point2i p2, Point2i p3, Point2i p4, IplImage* src, CvScalar color){


  Point2f point1 = Point2f(p1.x,p1.y);
  Point2f point2 = Point2f(p2.x,p2.y);
  Point2f point3 = Point2f(p3.x,p3.y);
  Point2f point4 = Point2f(p4.x,p4.y);


  //	cvLine(src, cvPoint(point1.x,point1.y),  cvPoint(point1.x,point1.y), color, 5, 8, 0);
  //	cvLine(src, cvPoint(point2.x,point2.y),  cvPoint(point2.x,point2.y), color, 5, 8, 0);
  //	cvLine(src, cvPoint(point3.x,point3.y),  cvPoint(point3.x,point3.y), color, 5, 8, 0);
  //	cvLine(src, cvPoint(point4.x,point4.y),  cvPoint(point4.x,point4.y), color, 5, 8, 0);

  float m1 = (point2.y - point1.y) / (point2.x - point1.x);
  float m2 = (point4.y - point3.y) / (point4.x - point3.x);


  cout << "M1 : " << m1 << "  M2 : " << m2  <<endl;

  Point2f hitp;
  hitp.x = ((m2*point4.x) - (m1*point2.x) + point2.y - point4.y) / ( m2 - m1);
  hitp.y = m1*(hitp.x - point2.x) + point2.y;


  //	cvLine(src,cvPoint(hitp.x,hitp.y),cvPoint(point1.x,point1.y),color);
  //	cvLine(src,cvPoint(hitp.x,hitp.y),cvPoint(point3.x,point3.y),color);

  return hitp;

}


bool touch_circle(Point2i p1, Point2i p2, Point2f circle , float r){

  Point2f point1 = Point2f(p1.x,p1.y);
  Point2f point2 = Point2f(p2.x,p2.y);

  float a = point1.y - point2.y;
  float b = point2.x - point1.x;
  float c = ( point2.x * (point2.y-point1.y) ) - ( point2.y * (point2.x -point1.x));

  float d =  fabs(a*circle.x + b*circle.y + c) / sqrt(a*a + b*b);

  if(d <= r)
    return true;
  else
    return false;

}

void draw_line(IplImage* src, vector<Point> line , CvScalar color){
  for(int j = 0; j<line.size(); j++){
    cvLine(src, cvPoint(line[j].x, line[j].y),  cvPoint(line[j].x, line[j].y), color, 4, 8, 0);
  }
}


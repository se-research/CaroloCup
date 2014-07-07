#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>

using namespace cv;
using namespace std;

void processImage(IplImage* img) {

  Mat src(img,false);

  if (src.empty()) {
    return;
  }

  Mat dst, cdst;
  Canny(src, dst, 50, 200, 3);
  //cvtColor(dst, cdst, CV_GRAY2BGR);

//  // 1
//  vector<Vec2f> lines;
//  HoughLines(dst, lines, 1, CV_PI/180, 100, 0, 0 );
//
//  for( size_t i = 0; i < lines.size(); i++ )
//  {
//    float rho = lines[i][0], theta = lines[i][1];
//    Point pt1, pt2;
//    double a = cos(theta), b = sin(theta);
//    double x0 = a*rho, y0 = b*rho;
//    pt1.x = cvRound(x0 + 1000*(-b));
//    pt1.y = cvRound(y0 + 1000*(a));
//    pt2.x = cvRound(x0 - 1000*(-b));
//    pt2.y = cvRound(y0 - 1000*(a));
//    line( cdst, pt1, pt2, Scalar(0,0,255), 3, CV_AA);
//  }
//
 // 2
 //vector<Vec4i> lines;
 //HoughLinesP(dst, lines, 1, CV_PI/180, 50, 50, 10 );
 //for( size_t i = 0; i < lines.size(); i++ )
 //{
   //Vec4i l = lines[i];
   //line( cdst, Point(l[0], l[1]), Point(l[2], l[3]), Scalar(0,0,255), 3, CV_AA);
 //}
}

int main(int argc, char** argv)
{
  CvCapture *capture;

  capture = cvCaptureFromCAM(0);
  if (!capture) {
    cerr << "Could not open real cameras; falling back to SHARED_IMAGE." << endl;
    return -1;
  }

  while (true) {
    if (cvGrabFrame(capture)) {
      IplImage* img;

      img = cvRetrieveFrame(capture);
      processImage(img);
      cvShowImage("WindowShowImage", img);
      cvWaitKey(10);
    }
  }

  return 0;
}



#include "cv.h"
#include "highgui.h"
#include <stdio.h>
#include "iostream"
#include <sys/time.h>

using namespace cv;
using namespace std;

long timeInMillis() {
  timeval tp;    //Creates a struct with hour, minute, second and microseconds
  gettimeofday(&tp, 0);    //Get the time

  return (tp.tv_usec / 1000);
}

int main() {

  Mat img0 = imread("/home/khashayar/Downloads/rgbb.png");
  Mat img1, blurr;
  cout << "DATA IS " << img0.data;
  
  cvtColor(img0, img1, CV_RGB2GRAY);

  GaussianBlur(img1, blurr, Size(11, 11), 11);
  Canny(blurr, blurr, 10, 100, 3);
  
  //	cout<< i;
  
  imshow("blurr", blurr);
  waitKey(1000);
  
     
  return 0;
}

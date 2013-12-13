#include "opencv2/opencv.hpp"
#include "LineDetector.h"
#include "math.h"

using namespace carolocup;

enum state_t {
  RUNNING,
  STOPPED,
  QUITING
} state;

/*static Scalar randomColor( RNG& rng )
{
  int icolor = (unsigned) rng;
  return Scalar( icolor&255, (icolor>>8)&255, (icolor>>16)&255 );
}*/

int main(int , char** argv)
{
  const char* filename = argv[1];
  VideoCapture cap;
  if(filename) {
    cap.open(filename);
  } else {
    cap.open(0);
  }

  if(!cap.isOpened()){
    return -1;
  }

  Config cfg;
  cfg.th1 = 120;
  cfg.th2 = 230;
  cfg.hlTh = THRESH_BINARY;
  cfg.caThVal = 300;
  cfg.caThMax = 100;
  cfg.caThTyp = 3;
  cfg.houghMinAngle = 20;
  cfg.houghMaxAngle = 160;
  cfg.houghStartVal = 25;
  cfg.houghMaxLines = 40;
  cfg.XTimesYMin = 1;
  cfg.XTimesYMax = 30;
  cfg.maxY = 200;

  namedWindow("config",1);
  //Dash properties
  createTrackbar("min times", "config", &cfg.XTimesYMin, 5);
  createTrackbar("max times", "config", &cfg.XTimesYMax, 40);
  createTrackbar("max y", "config", &cfg.maxY , 400);
  // Threshold
  createTrackbar("th1", "config", &cfg.th1, 255);
  createTrackbar("th2", "config", &cfg.th2, 255);

  Mat frame, dst;
  RNG rng( 0xFFFFFFFF );
  int avg_time=0, num_msmnt=0;
  while(state != QUITING) {
    if (state != STOPPED) {
	    cap >> frame; // get a new frame from camera
	    cvtColor( frame, frame, CV_BGR2GRAY );
	    flip(frame, frame, 0);
	    clock_t start = clock();
	    int w = frame.size().width;
	    int h = frame.size().height;
	    Mat getFirst = frame(cv::Rect(1, 7*h/16-1, w-1, 9*h/16-1));
	    //Mat getFirst = frame(cv::Rect(1, h/2-1, w-1, h/2-1));
	    dst = getFirst.clone();
	    
	    LineDetector road(getFirst, cfg, true, 1);
	    int ms = (difftime(clock(), start) / 1000);
	    cout << ms << "ms" << endl;
	    if(avg_time == 0) {
		avg_time = ms;
		num_msmnt = 1;
	    } else {
		avg_time = (avg_time * num_msmnt + ms) / (num_msmnt + 1);
		num_msmnt = (num_msmnt + 1) % 10;
	    }
	    cout << "avg_time: " << avg_time << "ms" << endl;

	    /*carolocup::Lines l = road.getLines();
	    Line dashed = l.dashedLine;
	    Line solidRight = l.rightLine;
	    Line solidLeft = l.leftLine;

	    line( dst, Point(dashed[0], dashed[1]), Point(dashed[2], dashed[3]), Scalar(0,255,0), 3, CV_AA);
	    line( dst, Point(solidRight[0], solidRight[1]), Point(solidRight[2], solidRight[3]), Scalar(255,0,0), 3, CV_AA);
	    line( dst, Point(solidLeft[0], solidLeft[1]), Point(solidLeft[2], solidLeft[3]), Scalar(0,0,255), 3, CV_AA);*/
	    //dst.setTo( Scalar(0,0,0));

	    //imshow("output", dst);
	      
    }
    // set state
    switch( waitKey(30) ) {
      case 'q':
        state = QUITING;
      break;
      case 's':
        if(state == STOPPED) {
          state = RUNNING;
        } else {
          state = STOPPED;
        }
      break;
    }
  }

  return 0;
}

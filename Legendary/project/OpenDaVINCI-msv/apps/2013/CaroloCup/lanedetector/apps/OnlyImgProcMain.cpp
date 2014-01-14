#include "opencv2/opencv.hpp"
#include "LineDetector.h"
#include "Transforms.h"
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
  cfg.maxY = 230;

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
	    carolocup::Lines lines = road.getLines();
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

	    Line dashed = lines.dashedLine;
	    Line solidRight = lines.rightLine;
	    Line solidLeft = lines.leftLine;
      /*CameraStruct cam;
      cam.focal = 489.0;
      cam.height = 0.15;
      cam.length = 0.08;
      cam.u0 = 183.0;
      cam.v0 = 144.0;
      cam.theta0 = -15;
      cam.gamma0 = 0;
      cam.size = Size(752, 480);
      Point2i img_pt_1, img_pt_2;
      Point2f real_pt_1, real_pt_2;
      //Test dashed line distance
      img_pt_1 = Point2i(dashed[0], dashed[1]);
      img_pt_2 = Point2i(dashed[2], dashed[3]);
      real_pt_1 = ipm2(img_pt_1, cam);
      real_pt_2 = ipm2(img_pt_1, cam);
      cout << "Distance of dashed line: " << sqrt(pow(real_pt_1.x-real_pt_2.x,2) + pow(real_pt_1.y-real_pt_2.y,2)) << endl;
      //Test left solid line distance
      img_pt_1 = Point2i(solidLeft[0], solidLeft[1]);
      img_pt_2 = Point2i(solidLeft[2], solidLeft[3]);
      real_pt_1 = ipm2(img_pt_1, cam);
      real_pt_2 = ipm2(img_pt_1, cam);
      cout << "Distance of left solid line: " << sqrt(pow(real_pt_1.x-real_pt_2.x,2) + pow(real_pt_1.y-real_pt_2.y,2)) << endl;
      //Test right solid line distance
      img_pt_1 = Point2i(solidRight[0], solidRight[1]);
      img_pt_2 = Point2i(solidRight[2], solidRight[3]);
      real_pt_1 = ipm2(img_pt_1, cam);
      real_pt_2 = ipm2(img_pt_1, cam);
      cout << "Distance of right solid line: " << sqrt(pow(real_pt_1.x-real_pt_2.x,2) + pow(real_pt_1.y-real_pt_2.y,2)) << endl;*/

	    //line( dst, Point(dashed[0], dashed[1]), Point(dashed[2], dashed[3]), 255, 3, CV_AA);
	    //line( dst, Point(solidRight[0], solidRight[1]), Point(solidRight[2], solidRight[3]), 0, 3, CV_AA);
	    //line( dst, Point(solidLeft[0], solidLeft[1]), Point(solidLeft[2], solidLeft[3]), 123, 3, CV_AA);
	    line( dst, lines.goalLine.p1, lines.goalLine.p2, 255, 3, CV_AA);
	    line( dst, lines.currentLine.p1, lines.currentLine.p2, 0, 3, CV_AA);
	    //dst.setTo( Scalar(0,0,0));

	    cout << "VP [x, y] : [" << lines.goalLine.p1.x << ", " << lines.goalLine.p1.y << "]" << endl;
	    cout << "Goal [x, y] : [" << lines.goalLine.p2.x << ", " << lines.goalLine.p2.y << "]" << endl;
	    cout << "Position [x, y] : [" << lines.currentLine.p2.x << ", " << lines.currentLine.p2.y << "]" << endl;
	    imshow("Final output", dst);
	      
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

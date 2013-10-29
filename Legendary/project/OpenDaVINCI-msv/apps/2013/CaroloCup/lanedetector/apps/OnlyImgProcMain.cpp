#include "opencv2/opencv.hpp"
#include "LineDetector.h"

using namespace carolocup;

enum state_t {
  RUNNING,
  STOPPED,
  QUITING
} state;

static Scalar randomColor( RNG& rng )
{
  int icolor = (unsigned) rng;
  return Scalar( icolor&255, (icolor>>8)&255, (icolor>>16)&255 );
}

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
  cfg.th1 = 40;
  cfg.th2 = 10;
  cfg.hlTh = 10;
  cfg.hlMaxLineGap = 1;
  cfg.hlMaxLineLength = 1;
  cfg.hlMaxLineLength = 1;
  cfg.caThVal = 200;
  cfg.caThMax = 200;
  cfg.caThTyp = 0;
  cfg.birdF = 900;
  cfg.birdDist = 100;
  cfg.birdAlpha = 17;
  cfg.birdBeta = 90;
  cfg.birdGamma = 90;
  cfg.dbEps = 17;
  cfg.dbMinPts = 5;
  cfg.dashMin = 15;
  cfg.dashMax = 40;
  cfg.dashWidth = 8;
  cfg.solidMin = 100;
  cfg.solidWidth = 30;

  //namedWindow("original",1);
  //namedWindow("thres",1);
  //namedWindow("canny",1);
  namedWindow("birdView",1);
  namedWindow("config",1);
  // Canny
  createTrackbar("th1", "config", &cfg.th1, 250);
  createTrackbar("th2", "config", &cfg.th2, 250);
  // HoughLineP
  createTrackbar("th", "config", &cfg.hlTh, 250);
  createTrackbar("maxLineLength", "config", &cfg.hlMaxLineLength, 250);
  createTrackbar("maxLineGap", "config", &cfg.hlMaxLineGap, 250);
  // Threshold
  createTrackbar("thValue", "config", &cfg.caThVal, 250);
  createTrackbar("binMax", "config", &cfg.caThMax, 250);
  createTrackbar("thType", "config", &cfg.caThTyp, 4);
  // BirdView
  //createTrackbar("f", "config", &cfg.birdF, 1500);
  //createTrackbar("dist", "config", &cfg.birdDist, 500);
  //createTrackbar("alpha", "config", &cfg.birdAlpha, 25);
  //createTrackbar("beta", "config", &cfg.birdBeta, 180);
  //createTrackbar("gamma", "config", &cfg.birdGamma, 360);
  // DBSCAN
  createTrackbar("eps", "config", &cfg.dbEps, 100);
  createTrackbar("minPts", "config", &cfg.dbMinPts, 100);
  createTrackbar("dashMin", "config", &cfg.dashMin, 100);
  createTrackbar("dashMax", "config", &cfg.dashMax, 200);
  createTrackbar("dashWidth", "config", &cfg.dashWidth, 25);
  createTrackbar("solidMin", "config", &cfg.solidMin, 200);
  createTrackbar("solidWidth", "config", &cfg.solidWidth, 50);

  Mat frame, dst;
  RNG rng( 0xFFFFFFFF );
  while(state != QUITING) {
    if (state != STOPPED) {
      cap >> frame; // get a new frame from camera
    }

    dst = frame.clone();
    dst.setTo( Scalar(0,0,0));

    LineDetector road(frame, cfg, true);
    Clusters* clusters = road.getClusters();

    for (vector<Cluster>::iterator it = clusters->begin(); it != clusters->end(); ++it) {
      Scalar color = randomColor(rng);
      for (vector<Point>::iterator it2 = it->begin(); it2 != it->end(); ++it2) {
        line( dst, *it2, *it2, color, 2, CV_AA);
      }
    }

    carolocup::Lines l = road.getLines();
    Line dashed = l.dashedLine;
    Line solidRight = l.rightLine;
    Line solidLeft = l.leftLine;

    line( dst, Point(dashed[0], dashed[1]), Point(dashed[2], dashed[3]), Scalar(0,255,0), 3, CV_AA);
    line( dst, Point(solidRight[0], solidRight[1]), Point(solidRight[2], solidRight[3]), Scalar(255,0,0), 3, CV_AA);
    line( dst, Point(solidLeft[0], solidLeft[1]), Point(solidLeft[2], solidLeft[3]), Scalar(0,0,255), 3, CV_AA);

    imshow("birdView", dst);

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

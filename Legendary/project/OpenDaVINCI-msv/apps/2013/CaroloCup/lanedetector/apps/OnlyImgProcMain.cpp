#include "opencv2/opencv.hpp"
#include "LineDetector.h"

using namespace carolocup;

struct Config {
  int th1, th2, hlTh, hlMaxLineGap, hlMaxLineLength, caThVal, caThMax, caThTyp,
      birdF, birdDist, birdAlpha, birdBeta, birdGamma, dbEps, dbMinPts,
      dashMin, dashMax, dashWidth, solidMax, solidWidth;
};

enum state_t {
  RUNNING,
  STOPPED,
  QUITING
} state;

Mat translate(Mat& source, Config& cfg)
{
  double alpha = ((double)cfg.birdAlpha-90.)*CV_PI/180 ;
  double beta = ((double)cfg.birdBeta-90.)*CV_PI/180 ;
  double gamma = ((double)cfg.birdGamma-90.)*CV_PI/180 ;
  double f = cfg.birdF;
  double dist = cfg.birdDist;

  double w = source.size().width;
  double h = source.size().height;

  // Projection 2D -> 3D matrix
  Mat A1=(Mat_<double>(4,3)<<
      1,0,-w/2,
      0,1,-h/2,
      0,0,0,
      0,0,1);

  // Rotation matrices around the X,Y,Z axis
  Mat RX=(Mat_<double>(4,4)<<
      1,0,0,0,
      0,cos(alpha),-sin(alpha),0,
      0,sin(alpha),cos(alpha),0,
      0,0,0,1);

  Mat RY=(Mat_<double>(4,4)<<
      cos(beta),0,-sin(beta),0,
      0,1,0,0,
      sin(beta),0,cos(beta),0,
      0,0,0,1);

  Mat RZ=(Mat_<double>(4,4)<<
      cos(gamma),-sin(gamma),0,0,
      sin(gamma),cos(gamma),0,0,
      0,0,1,0,
      0,0,0,1);

  // Composed rotation matrix with (RX,RY,RZ)
  Mat R=RX*RY*RZ ;

  // Translation matrix on the Z axis change dist will change the height
  Mat T=(Mat_<double>(4,4)<<
      1,0,0,0,
      0,1,0,0,
      0,0,1,dist,
      0,0,0,1);

  // Camera Intrisecs matrix 3D -> 2D
  Mat A2=(Mat_<double>(3,4)<<
      f,0,w/2,0,
      0,f,h/2,0,
      0,0,1,0);

  // Final and overall transformation matrix
  Mat transfo=A2*(T*(R*A1));

  return transfo;
}

static Scalar randomColor( RNG& rng )
{
  int icolor = (unsigned) rng;
  return Scalar( icolor&255, (icolor>>8)&255, (icolor>>16)&255 );
}

void drawLines(Mat& src, Mat& dst, Config& cfg) {
  vector<Vec4i> lines;
  RNG rng( 0xFFFFFFFF );

  HoughLinesP(src, lines, 1, CV_PI/180, cfg.hlTh, cfg.hlMaxLineLength, cfg.hlMaxLineGap );

  LineDetector road(lines, cfg.dbEps, cfg.dbMinPts, cfg.dashMin, cfg.dashMax, cfg.dashWidth, cfg.solidMax, cfg.solidWidth);
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
  cfg.solidMax = 100;
  cfg.solidWidth = 30;

  //int th1 = 40, th2 = 10; // Threshold
  //int th = 10, rho = 1, theta = 180, maxLineGap = 1, maxLineLength = 1; // HoughLineP trans
  //int thVal = 200, thMax = 200, thTyp = 0; // Canny
  //int birdF = 900, birdDist = 100, birdAlpha = 17, birdBeta = 90, birdGamma = 90;
  //int eps = 17, minPts = 5;

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
  createTrackbar("solidMax", "config", &cfg.solidMax, 200);
  createTrackbar("solidWidth", "config", &cfg.solidWidth, 50);

  Mat frame;
  while(state != QUITING) {
    Mat original,thres,canny;
    if (state != STOPPED) {
      cap >> frame; // get a new frame from camera
    }
    original = frame.clone();
    cvtColor(original, thres, CV_BGR2GRAY);
    threshold( thres, thres, cfg.caThVal, cfg.caThMax, cfg.caThTyp );
    GaussianBlur(thres, canny, Size(7,7), 1.5, 1.5);
    Canny(canny, canny, cfg.th1, cfg.th2);

    //drawLines(canny,original);
    //drawCurves(canny,original);

    Mat birdView, birdViewHough;
    warpPerspective(
        canny,
        birdView,
        translate(canny,cfg),
        canny.size(),
        INTER_CUBIC|WARP_INVERSE_MAP);
    birdViewHough = original.clone(); // should be optimized
    birdViewHough.setTo( Scalar(0,0,0));
    drawLines(birdView,birdViewHough,cfg);

    //imshow("original", original);
    //imshow("thres", thres);
    //imshow("canny", canny);
    imshow("birdView", birdViewHough);

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
        //state = QUITING;
  }

  return 0;
}

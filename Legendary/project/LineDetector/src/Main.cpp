#include "opencv2/opencv.hpp"
#include "LineDetector.h"
#include "LineDetectorTypes.h"

using namespace cv;

int th1 = 40, th2 = 10; // Threshold
int th = 10, rho = 1, theta = 180, maxLineGap = 1, maxLineLength = 1; // HoughLineP trans
int thVal = 200, thMax = 200, thTyp = 0; // Canny
int rows = 16; // HoughCircle trans
int birdF = 900, birdDist = 100, birdAlpha = 17, birdBeta = 90, birdGamma = 90;
int eps = 35, minPts = 5;

enum state_t {
  RUNNING,
  STOPPED,
  QUITING
} state;

Mat translate(Mat& source)
{
  double alpha = ((double)birdAlpha-90.)*CV_PI/180 ;
  double beta = ((double)birdBeta-90.)*CV_PI/180 ;
  double gamma = ((double)birdGamma-90.)*CV_PI/180 ;
  double f = birdF;
  double dist = birdDist;

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


// Attila: Not used right now
void drawCurves(Mat& src, Mat& dst) {
  vector<Vec3f> circles;

  // Apply the Hough Transform to find the circles
  HoughCircles( src, circles, CV_HOUGH_GRADIENT, 1, rows/8, 200, 100, 5, 90 );

  // Draw the circles detected
  for( size_t i = 0; i < circles.size(); i++ )
  {
      Point center(cvRound(circles[i][0]), cvRound(circles[i][1]));
      int radius = cvRound(circles[i][2]);
      // circle center
      circle( src, center, 3, Scalar(0,255,0), -1, 8, 0 );
      // circle outline
      circle( src, center, radius, Scalar(0,0,255), 3, 8, 0 );
  }
}

static Scalar randomColor( RNG& rng )
{
  int icolor = (unsigned) rng;
  return Scalar( icolor&255, (icolor>>8)&255, (icolor>>16)&255 );
}

void drawLines(Mat& src, Mat& dst) {
  vector<Vec4i> lines;
  RNG rng( 0xFFFFFFFF );

  HoughLinesP(src, lines, rho, CV_PI/theta, th, maxLineLength, maxLineGap );

  LineDetector road(lines, eps, minPts);
  Clusters* clusters = road.getClusters();

  for (vector<Cluster>::iterator it = clusters->begin(); it != clusters->end(); ++it) {
    Scalar color = randomColor(rng);
    for (vector<Point>::iterator it2 = it->begin(); it2 != it->end(); ++it2) {
      line( dst, *it2, *it2, color, 2, CV_AA);
    }
  }

  //Line solid = road.getSolidLine();
  //Line dash = road.getDashLine();
  //line( dst, Point(dash[0], dash[1]), Point(dash[2], dash[3]), Scalar(0,255,0), 3, CV_AA);

  //for( size_t i = 0; i < lines.size(); i++ ) {
    //Vec4i l = lines[i];
    //line( dst, Point(l[0], l[1]), Point(l[0], l[1]), Scalar(0,0,255), 2, CV_AA);
    //line( dst, Point(l[2], l[3]), Point(l[2], l[3]), Scalar(0,255,0), 2, CV_AA);
  //}

}

int main(int argc, char** argv)
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

  namedWindow("original",1);
  //namedWindow("thres",1);
  //namedWindow("canny",1);
  namedWindow("birdView",1);
  // Canny
  createTrackbar("th1", "canny", &th1, 250);
  createTrackbar("th2", "canny", &th2, 250);
  // HoughLineP
  createTrackbar("th", "original", &th, 250);
  createTrackbar("rho", "original", &rho, 250);
  createTrackbar("th", "original", &th, 250);
  createTrackbar("theta", "original", &theta, 250);
  createTrackbar("maxLineLength", "original", &maxLineLength, 250);
  createTrackbar("maxLineGap", "original", &maxLineGap, 250);
  // Threshold
  createTrackbar("thValue", "thres", &thVal, 250);
  createTrackbar("binMax", "thres", &thMax, 250);
  createTrackbar("thType", "thres", &thTyp, 4);
  // Circle
  //createTrackbar("thValue","circle", &thVal, 250);
  //createTrackbar("binMax", "circle", &thMax, 250);
  //createTrackbar("thType", "circle", &thTyp, 4);
  // BirdView
  createTrackbar("f", "birdView", &birdF, 1500);
  createTrackbar("dist", "birdView", &birdDist, 500);
  createTrackbar("alpha", "birdView", &birdAlpha, 25);
  createTrackbar("beta", "birdView", &birdBeta, 180);
  createTrackbar("gamma", "birdView", &birdGamma, 360);
  // DBSCAN
  createTrackbar("eps", "birdView", &eps, 100);
  createTrackbar("minPts", "birdView", &minPts, 100);

  Mat frame;
  while(state != QUITING) {
    Mat original,thres,canny;
    if (state != STOPPED) {
      cap >> frame; // get a new frame from camera
    }
    original = frame.clone();
    cvtColor(original, thres, CV_BGR2GRAY);
    threshold( thres, thres, thVal, thMax,thTyp );
    GaussianBlur(thres, canny, Size(7,7), 1.5, 1.5);
    Canny(canny, canny, th1, th2);

    drawLines(canny,original);
    //drawCurves(canny,original);

    Mat birdView, birdViewHough;
    warpPerspective(
        canny,
        birdView,
        translate(canny),
        canny.size(),
        INTER_CUBIC|WARP_INVERSE_MAP);
    birdViewHough = original.clone(); // should be optimized
    birdViewHough.setTo( Scalar(0,0,0));
    drawLines(birdView,birdViewHough);

    imshow("original", original);
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

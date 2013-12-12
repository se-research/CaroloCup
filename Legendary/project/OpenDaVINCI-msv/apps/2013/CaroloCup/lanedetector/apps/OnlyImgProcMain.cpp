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
  cfg.th1 = 170;
  cfg.th2 = 230;
  cfg.hlTh = THRESH_BINARY;
  cfg.caThVal = 300;
  cfg.caThMax = 100;
  cfg.caThTyp = 3;
  cfg.houghMinAngle = 20;
  cfg.houghMaxAngle = 160;
  cfg.houghStartVal = 25;
  cfg.houghMaxLines = 40;

  int minArea = 1;
  int maxArea = 30;
  int maxY = 200;

  //namedWindow("original",1);
  //namedWindow("thres",1);
  //namedWindow("canny",1);
  //namedWindow("birdView",1);
  namedWindow("config",1);
  createTrackbar("min times", "config", &minArea, 5);
  createTrackbar("max times", "config", &maxArea, 40);
  createTrackbar("max y", "config", &maxY, 400);
  // Canny
  createTrackbar("th1", "config", &cfg.th1, 255);
  createTrackbar("th2", "config", &cfg.th2, 255);
  // HoughLineP
  //createTrackbar("th", "config", &cfg.hlTh, 250);
  //createTrackbar("maxLineLength", "config", &cfg.hlMaxLineLength, 250);
  //createTrackbar("maxLineGap", "config", &cfg.hlMaxLineGap, 250);
  /* Threshold
  createTrackbar("thValue", "config", &cfg.caThVal, 250);
  createTrackbar("binMax", "config", &cfg.caThMax, 250);
  createTrackbar("thType", "config", &cfg.caThTyp, 4);*/
  // BirdView
  /*createTrackbar("f", "config", &cfg.birdF, 1500);
  createTrackbar("dist", "config", &cfg.birdDist, 500);
  createTrackbar("alpha", "config", &cfg.birdAlpha, 25);
  createTrackbar("beta", "config", &cfg.birdBeta, 180);
  createTrackbar("gamma", "config", &cfg.birdGamma, 360);
  // DBSCAN
  createTrackbar("eps", "config", &cfg.dbEps, 100);
  createTrackbar("minPts", "config", &cfg.dbMinPts, 100);
  createTrackbar("dashMin", "config", &cfg.dashMin, 100);
  createTrackbar("dashMax", "config", &cfg.dashMax, 200);
  createTrackbar("dashWidth", "config", &cfg.dashWidth, 25);
  createTrackbar("solidMin", "config", &cfg.solidMin, 200);
  createTrackbar("solidWidth", "config", &cfg.solidWidth, 50);*/

  Mat frame, dst;
  vector<vector<Point> > contours;
  vector<Vec4i> hierarchy;
  RNG rng( 0xFFFFFFFF );
  int avg_time=0, num_msmnt=0;
  while(state != QUITING) {
    if (state != STOPPED) {
      cap >> frame; // get a new frame from camera
    }
    cvtColor( frame, frame, CV_BGR2GRAY );
    flip(frame, frame, 0);
    clock_t start = clock();
    int w = frame.size().width;
    int h = frame.size().height;
    Mat getFirst = frame(cv::Rect(1, 3*h/8-1, w-1, 5*h/8-1));
    //Mat getFirst = frame(cv::Rect(1, h/2-1, w-1, h/2-1));
    dst = getFirst.clone();
    vector<vector<Point> > contours;
    vector<Vec4i> hierarchy;
    /// Detect edges using Threshold
    threshold( getFirst, getFirst, cfg.th1, cfg.th2, cfg.hlTh );
    cv::Canny(getFirst, getFirst, cfg.caThVal, cfg.caThMax, cfg.caThTyp);
    /// Find contours
    findContours( getFirst, contours, hierarchy, CV_RETR_TREE, CV_CHAIN_APPROX_SIMPLE, Point(0, 0) );

    /// Approximate contours to polygons + get bounding rects and circles
    vector<vector<Point> > contours_poly( contours.size() );
    vector<RotatedRect> boundRect(contours.size());
    vector<RotatedRect> solidRect(contours.size());
    //vector<Point2f>center( contours.size() );
    //vector<float>radius( contours.size() );
    int cntDash = 0;
    int cntSolid = 0;
    for( int i = 0; i < contours.size(); i++ )
    {
        approxPolyDP( Mat(contours[i]), contours_poly[i], 3, true );
	RotatedRect rect = minAreaRect(contours_poly[i]);
        Point2f rect_points[4]; rect.points( rect_points );
	//cout << "Angle: " << rect.angle << endl;
	float sizeX = 0, sizeY = 0, sizeR = 0;
        for( int j = 0; j < 4; j++ ) {
          //cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y << "]" << endl;
          sizeR = cv::sqrt(cv::pow((rect_points[j].x - rect_points[(j+1)%4].x), 2) + cv::pow((rect_points[j].y - rect_points[(j+1)%4].y), 2)); 
	  //cout << "Size:" << sizeR << endl;
	  if(sizeX == 0) {
		sizeX = sizeR;	
	  } else if(sizeY == 0 && sizeR != sizeX) {
		sizeY = sizeR;
	  }
        }
	if(sizeX > sizeY) {
		sizeR = sizeX;
		sizeX = sizeY;
		sizeY = sizeR;
	} 
	//cout << "Sizes [x,y] = [" << sizeX << "," << sizeY << "]" << endl;
	if(sizeY > minArea*sizeX && sizeY < maxArea*sizeX && sizeY < maxY ) {
             boundRect[cntDash] = rect;
	     cntDash++;
	} else if(sizeY > 4*minArea*sizeX && sizeY > (maxY/2)){
	     solidRect[cntSolid] = rect;
	     cntSolid++;
	     cout << "Angle: " << rect.angle << endl;
	}
        //minEnclosingCircle( (Mat)contours_poly[i], center[i], radius[i] );
    }
    for(int j=0; j < cntSolid; j++) {
	if(solidRect[j].angle < -10) {
		float a = M_PI * (90 + solidRect[j].angle) / 180;
		float b = solidRect[j].center.y - solidRect[j].center.x * a;
		cout << "Equation [a,b]: [" << a << "," << b << "]" << endl;
		for(int l=0; l < cntDash; l++) {
			float res = a*boundRect[l].center.x + b;
			cout << "[res, y] = [" << res << "," << boundRect[l].center.y << "]" << endl;
			cout << "[x, y] = [" << boundRect[l].center.x << "," << boundRect[l].center.y << "]" << endl;
			if(res > boundRect[l].center.y) {
				boundRect[l] = boundRect[cntDash-1];
				cntDash--;
				l--;
				cout<< cntDash <<endl;
			}
		}
 	}
    }
    cout << "Rects: " << (cntDash - 1) << endl;
    //LineDetector road(frame, cfg, true, 1);
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
    /*Clusters* clusters = road.getClusters();

    for (vector<Cluster>::iterator it = clusters->begin(); it != clusters->end(); ++it) {
      Scalar color = randomColor(rng);
      for (vector<Point>::iterator it2 = it->begin(); it2 != it->end(); ++it2) {
        line( dst, *it2, *it2, color, 2, CV_AA);
      }
    }*/

    /*carolocup::Lines l = road.getLines();
    Line dashed = l.dashedLine;
    Line solidRight = l.rightLine;
    Line solidLeft = l.leftLine;

    line( dst, Point(dashed[0], dashed[1]), Point(dashed[2], dashed[3]), Scalar(0,255,0), 3, CV_AA);
    line( dst, Point(solidRight[0], solidRight[1]), Point(solidRight[2], solidRight[3]), Scalar(255,0,0), 3, CV_AA);
    line( dst, Point(solidLeft[0], solidLeft[1]), Point(solidLeft[2], solidLeft[3]), Scalar(0,0,255), 3, CV_AA);*/


    /// Draw polygonal contour + bonding rects + circles
    //Mat drawing = Mat::zeros( threshold_output.size(), CV_8UC3 );
    /*for( int i = 0; i< contours.size(); i++ )
    {
       Scalar color = Scalar( rng.uniform(0, 255), rng.uniform(0,255), rng.uniform(0,255) );
       //drawContours( dst, contours_poly, i, color, 1, 8, vector<Vec4i>(), 0, Point() );
       Point2f rect_points[4]; boundRect[i].points( rect_points );
       Point2f rect_pts[4]; solidRect[i].points( rect_pts );
       for( int j = 0; j < 4; j++ )
          line( dst, rect_points[j], rect_points[(j+1)%4], 255, 1, 8 );
       for( int j = 0; j < 4; j++ )
          line( dst, rect_pts[j], rect_pts[(j+1)%4], 0, 1, 8 );
       //rectangle( dst, boundRect[i].tl(), boundRect[i].br(), color, 2, 8, 0 );
       //circle( dst, center[i], (int)radius[i], color, 2, 8, 0 );
    }*/
    for(int i = 0; i < cntDash; i++) {
	Point2f rect_points[4]; boundRect[i].points( rect_points );
	for( int j = 0; j < 4; j++ )
          line( dst, rect_points[j], rect_points[(j+1)%4], 255, 1, 8 );
    }
    for(int i = 0; i < cntSolid; i++) {
	Point2f rect_points[4]; solidRect[i].points( rect_points );
	for( int j = 0; j < 4; j++ )
          line( dst, rect_points[j], rect_points[(j+1)%4], 0, 1, 8 );
    }
    Point p1, p2;
    p1.x = 10;
    p1.y = 10;
    p2.x = 20;
    p2.y = 20;
    line( dst, p1, p2, 124, 1, 8 );
    //dst.setTo( Scalar(0,0,0));

    imshow("output", dst);

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

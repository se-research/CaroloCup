#include <highgui.h>
#include <cv.h>
#include <cxcore.h>
#include <math.h>
#include <vector>
#include <stdio.h>

#include <iostream>

using namespace cv;
using namespace std;

int main(int argc, char* argv[]) {

  if(argc != 4) return -1;
  // INPUT PARAMETERS:
  //
  int board_w = atoi(argv[1]); //inner corners per row
  int board_h = atoi(argv[2]); //inner corners per column
  int board_n = board_w * board_h;
  CvSize board_sz = cvSize( board_w, board_h );

  //Hard coded intrinsics for the camera
  Mat intrinsicMat = (Mat_<double>(3, 3) <<
      418.7490, 0., 236.8528,
      0.,558.6650,322.7346,
      0., 0., 1.);

  //Hard coded distortions for the camera
  CvMat* distortion = cvCreateMat(1, 4, CV_32F);
  cvmSet(distortion, 0, 0, -0.0019);
  cvmSet(distortion, 0, 1, 0.0161);
  cvmSet(distortion, 0, 2, 0.0011);
  cvmSet(distortion, 0, 3, -0.0016);

  IplImage* image = 0;
  IplImage* gray_image = 0;

  if( (image = cvLoadImage(argv[3])) == 0 ) {
    printf("Error: Couldn’t load %s\n",argv[3]);
    return -1;
  }

  gray_image = cvCreateImage( cvGetSize(image), 8, 1 );
  cvCvtColor(image, gray_image, CV_BGR2GRAY );
  // UNDISTORT OUR IMAGE
  //
  IplImage* mapx = cvCreateImage( cvGetSize(image), IPL_DEPTH_32F, 1 );
  IplImage* mapy = cvCreateImage( cvGetSize(image), IPL_DEPTH_32F, 1 );

  CvMat intrinsic (intrinsicMat);

  //This initializes rectification matrices
  //
  cvInitUndistortMap(
      &intrinsic,
      distortion,
      mapx,
      mapy
      );


  IplImage *t = cvCloneImage(image);
  // Rectify our image
  //

  cvRemap( t, image, mapx, mapy );
  // GET THE CHESSBOARD ON THE PLANE
  //

  cvNamedWindow("Chessboard");
  CvPoint2D32f* corners = new CvPoint2D32f[ board_n ];
  int corner_count = 0;
  int found = cvFindChessboardCorners(
      image,
      board_sz,
      corners,
      &corner_count,
      CV_CALIB_CB_ADAPTIVE_THRESH | CV_CALIB_CB_FILTER_QUADS
      );
  if(!found){
    printf("Couldn’t aquire chessboard on %s, "
        "only found %d of %d corners\n",
        argv[3],corner_count,board_n
        );
    return -1;
  }
  //Get Subpixel accuracy on those corners:
  cvFindCornerSubPix(
      gray_image,
      corners,
      corner_count,
      cvSize(11,11),
      cvSize(-1,-1),
      cvTermCriteria( CV_TERMCRIT_EPS | CV_TERMCRIT_ITER, 30, 0.1 )
      );

  //GET THE IMAGE AND OBJECT POINTS:
  // We will choose chessboard object points as (r,c):
  // (0,0), (board_w-1,0), (0,board_h-1), (board_w-1,board_h-1).
  //

  CvPoint2D32f objPts[4], imgPts[4];
  imgPts[0] = corners[0];
  imgPts[1] = corners[board_w-1];
  imgPts[2] = corners[(board_h-1)*board_w];
  imgPts[3] = corners[(board_h-1)*board_w + board_w-1];

  objPts[0].x = 0; objPts[0].y = 0;
  objPts[1].x = board_w -1; objPts[1].y = 0;
  objPts[2].x = 0; objPts[2].y = board_h -1;
  objPts[3].x = board_w -1; objPts[3].y = board_h -1;


  // DRAW THE POINTS in order: B,G,R,YELLOW
  //
  cvCircle( image, cvPointFrom32f(imgPts[0]), 9, CV_RGB(0,0,255), 3); //blue
  cvCircle( image, cvPointFrom32f(imgPts[1]), 9, CV_RGB(0,255,0), 3); //green
  cvCircle( image, cvPointFrom32f(imgPts[2]), 9, CV_RGB(255,0,0), 3); //red
  cvCircle( image, cvPointFrom32f(imgPts[3]), 9, CV_RGB(255,255,0), 3); //yellow
  // DRAW THE FOUND CHESSBOARD
  //

  cvDrawChessboardCorners(
      image,
      board_sz,
      corners,
      corner_count,
      found
      );
  cvShowImage( "Chessboard", image );
  // FIND THE HOMOGRAPHY
  //
  CvMat *H = cvCreateMat( 3, 3, CV_32F);
  cvGetPerspectiveTransform( objPts, imgPts, H);
  Mat homography = H;
  cvSave("Homography.xml",H); //We can reuse H for the same camera mounting

  /**********************GENERATING 3X4 MATRIX***************************/

  // LET THE USER ADJUST THE Z HEIGHT OF THE VIEW
  //
  float Z = 23;
  int key = 0;
  IplImage *birds_image = cvCloneImage(image);
  cvNamedWindow("Birds_Eye");
  // LOOP TO ALLOW USER TO PLAY WITH HEIGHT:
  //
  // escape key stops
  //
  while(key != 27) {
    // Set the height
    //
    CV_MAT_ELEM(*H,float,2,2) = Z;
    // COMPUTE THE FRONTAL PARALLEL OR BIRD’S-EYE VIEW:
    // USING HOMOGRAPHY TO REMAP THE VIEW
    //
    cvWarpPerspective(
        image,
        birds_image,
        H,
        CV_INTER_LINEAR | CV_WARP_INVERSE_MAP | CV_WARP_FILL_OUTLIERS
        );
    cvShowImage( "Birds_Eye", birds_image );
    imwrite("/home/lee/bird.jpg", birds_image);

    key = cvWaitKey();
    if(key == 'u') Z += 0.5;
    if(key == 'd') Z -= 0.5;
  }
  return 0;
}

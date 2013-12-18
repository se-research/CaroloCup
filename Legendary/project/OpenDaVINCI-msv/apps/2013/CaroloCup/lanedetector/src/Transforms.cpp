/*
 * Transforms.cpp
 *
 *  Created on: 14 dec 2013
 *      Author: fredrik
 */
#include "Transforms.h"

namespace carolocup {

Point2f ipm(Point2i img_pt, CameraStruct cam) {
    float f = cam.focal;
    float u0 = cam.u0;
    float v0 = cam.v0;
    float theta0 = cam.theta0;
    float gamma0 = cam.gamma0;
    float h = cam.height;
    float a = cam.length;
    float u = img_pt.x, v = img_pt.y;
    int m = cam.size.height, n = cam.size.width;
    /*
    float u_prim = u + n/2, v_prim = v + m/2;
    float a11 = -u_prim*sin(gamma0)*cos(theta0)-f*cos(gamma0)*cos(theta0);
    float a12 = -u_prim*sin(gamma0)*sin(theta0)-f*cos(gamma0)*sin(theta0);
    float a21 = -v_prim*sin(gamma0)*cos(theta0)+f*sin(theta0);
    float a22 = -v_prim*sin(gamma0)*sin(theta0)+f*cos(theta0);
    float b1 = u_prim*h*cos(gamma0)-f*h*sin(gamma0), b2 = v_prim*h*cos(gamma0);
    matrix<double,2,2> A;
    matrix<double,2,1> b, xy;
    A = a11, a12, a21, a22; b = b1, b2;
    xy = inv(A)*b;
    return Point2f(xy(0), xy(1));
    */
    Mat K = (Mat_<double>(3,3) << f, 0, u0, 0, f, v0, 0, 0, 1);
    Mat R = (Mat_<double>(3,3) << 1, 0, 0, 0, cos(theta0), -sin(theta0), 0, sin(theta0), cos(theta0));
    Mat tc1 = (Mat_<double>(3,1) << 0, 0, h);
    Mat tc2 = (Mat_<double>(3,1) << 0, a, 0);
    Mat x = (Mat_<double>(3,1) << u, v, 1);
    Mat X = (K*R).inv()*(x + tc2 - K*tc1);
    return Point2f(X.at<double>(0,0), X.at<double>(1,0));
}

Point2f ipm2(Point2i img_pt, CameraStruct cam) {
	Mat Tr = getBirdTransMatrix(cam);
	Mat x = (Mat_<double>(3,1) << img_pt.x, img_pt.y, 1);
	Mat X = Tr*x;
	return Point2f(X.at<double>(0,0), X.at<double>(1,0));
}

Mat getBirdTransMatrix(CameraStruct cam) {
    //double alpha = ((double)cam.theta0-90.)*CV_PI/180 ;
	double alpha = cam.theta0*CV_PI/180;
    //double beta = ((double)cam.theta0)*CV_PI/180;
    double beta = 0*CV_PI/180;
    double gamma = cam.gamma0-90*CV_PI/180;
    double f = cam.focal;
    double dist = cam.height;
    double a = cam.length;

    double w = cam.size.width;
    double h = cam.size.height;
    double u0 = cam.u0;
    double v0 = cam.v0;

    // Projection 2D -> 3D matrix
    Mat A1=(Mat_<double>(4,3)<<
        1,0,-u0,
        0,1,-v0,
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
    Mat TZ=(Mat_<double>(4,4)<<
        1,0,0,0,
        0,1,0,0,
        0,0,1,dist,
        0,0,0,1);
    Mat TCam = (Mat_<double>(4,4) <<
    	1,0,0,0,
    	0,1,0,a,
    	0,0,1,0,
    	0,0,0,1);

    // Camera Intrisecs matrix 3D -> 2D
    Mat A2=(Mat_<double>(3,4)<<
        f,0,u0,0,
        0,f,v0,0,
        0,0,1,0);

    // Final and overall transformation matrix
    return A2*(TZ*(R*(TCam*A1)));
}
}

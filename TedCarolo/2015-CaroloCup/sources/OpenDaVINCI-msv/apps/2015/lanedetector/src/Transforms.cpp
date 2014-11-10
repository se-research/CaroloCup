/*
 * Transforms.cpp
 *
 *  Created on: 14 dec 2013
 *      Author: fredrik
 */
#include "Transforms.h"

namespace msv {

Point2f ipm(Point2i img_pt, CameraStruct cam) {
    float f = cam.focal;
    float u0 = cam.u0;
    float v0 = cam.v0;
    float alpha = cam.alpha;
    //float gamma = cam.gamma;
    float h = cam.height;
    float a = cam.length;
    float u = img_pt.x, v = img_pt.y;
    //int m = cam.size.height, n = cam.size.width;
    /*
    float u_prim = u + n/2, v_prim = v + m/2;
    float a11 = -u_prim*sin(gamma)*cos(alpha)-f*cos(gamma)*cos(alpha);
    float a12 = -u_prim*sin(gamma)*sin(alpha)-f*cos(gamma)*sin(alpha);
    float a21 = -v_prim*sin(gamma)*cos(alpha)+f*sin(alpha);
    float a22 = -v_prim*sin(gamma)*sin(alpha)+f*cos(alpha);
    float b1 = u_prim*h*cos(gamma)-f*h*sin(gamma), b2 = v_prim*h*cos(gamma);
    matrix<double,2,2> A;
    matrix<double,2,1> b, xy;
    A = a11, a12, a21, a22; b = b1, b2;
    xy = inv(A)*b;
    return Point2f(xy(0), xy(1));
    */
    Mat K = (Mat_<double>(3,3) << f, 0, u0, 0, f, v0, 0, 0, 1);
    Mat R = (Mat_<double>(3,3) << 1, 0, 0, 0, cos(alpha), -sin(alpha), 0, sin(alpha), cos(alpha));
    Mat tc1 = (Mat_<double>(3,1) << 0, 0, h);
    Mat tc2 = (Mat_<double>(3,1) << 0, a, 0);
    Mat x = (Mat_<double>(3,1) << u, v, 1);
    Mat X = (K*R).inv()*(x + tc2 - K*tc1);
    return Point2f(X.at<double>(0,0)/X.at<double>(0,2), X.at<double>(1,0)/X.at<double>(0,2));
}

Point2f ipm2(Point2i img_pt, CameraStruct cam) {
	Mat Tr = getBirdTransMatrix(cam);
	Mat x = (Mat_<double>(3,1) << img_pt.x, img_pt.y, 1);
	Mat X = Tr*x;
	return Point2f(X.at<double>(0,0), X.at<double>(1,0));
}

Point2f ipm3(Point2i img_pt, CameraStruct cam) {
	float h = cam.height;
	float theta = cam.alpha*CV_PI/180;
	float gamma = cam.gamma*CV_PI/180;
	float alphaU = atan2(cam.u0, cam.focal);
	float alphaV = atan2(cam.v0, cam.focal);
	float m = cam.size.height, n = cam.size.width;
	float u = img_pt.x, v = img_pt.y;
	float x = h/tan(theta-alphaU + u*2*alphaU/(m-1))*cos(gamma-alphaV + v*2*alphaV/(n-1));
	float z = h/tan(theta-alphaU + u*2*alphaU/(m-1))*sin(gamma-alphaV + v*2*alphaV/(n-1));
	return Point2f(x, z);
}

Mat getBirdTransMatrix(CameraStruct cam) {
    double alpha = ((double)cam.alpha-90.)*CV_PI/180 ;
    //double alpha = -cam.alpha*CV_PI/180;
    double beta = ((double)cam.beta-90)*CV_PI/180;
    //double beta = 0*CV_PI/180;
    double gamma = ((double)cam.gamma-90.)*CV_PI/180;
    double f = cam.focal;
    double f2 = cam.focal2;
    double dist = cam.height;
    double a = cam.length;

    //double w = cam.size.width;
    //double h = cam.size.height;
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
    Mat R = RX * RY * RZ;

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
    Mat A2=(Mat_<double>(3,4) <<
        f,0,u0,0,
        0,f2,v0,0,
        0,0,1,0);

    // Final and overall transformation matrix
    return A2*(TZ*(R*(A1)));
}

Point2f ipmFinal(Point2i p, CameraStruct cam) {
	p.x = p.x - cam.size.width/2;
	p.y = cam.size.height - p.y;
	Mat camera_matrix = getBirdTransMatrix(cam);

	double u, v, w;
	Point2f res;
	w = 1 / (camera_matrix.at<double>(2,0)*p.x + camera_matrix.at<double>(2,1)*p.y + camera_matrix.at<double>(2,2));
	u = w*p.x;
	v = w*p.y;

	res.x = camera_matrix.at<double>(0,0)*u + camera_matrix.at<double>(0,1)*v + camera_matrix.at<double>(0,2)*w;
	res.y = camera_matrix.at<double>(1,0)*u + camera_matrix.at<double>(1,1)*v + camera_matrix.at<double>(1,2)*w;

	return res;
}

}

#ifndef CAMERA_H
#define CAMERA_H

#include <opencv/cv.h>
using namespace cv;

bool init_camera();
bool get_image(char*& img);
bool deinit_camera();
#endif

#ifndef CAMERA_H
#define CAMERA_H

#include <opencv/cv.h>
using namespace cv;

void init_camera();
char* get_image();
void deinit_camera();
#endif

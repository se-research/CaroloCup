/*
 * RoadModel.cpp
 *
 *  Created on: Apr 8, 2013
 *      Author: Oscar Muchow
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <iostream>
#include <math.h>
#include <opencv/cv.h>
#include <opencv/highgui.h>

#include "core/data/Constants.h"
#include "core/base/KeyValueConfiguration.h"

#include "RoadModel.h"

#define THRESH 150 // Threshold value for PointSearch Left and Right
#define OFFSET 10 // Value to be use'd in calculations for finding lanes
namespace msv {

  using namespace cv;
  using namespace std;
  using namespace core::data;

  RoadModel::RoadModel() :
    steeringAngle(0),
    oldAngle(0),
    roadMiddle(0) { }

  RoadModel::~RoadModel() {

  }
  /* This is the main algorithm for finding the correct steering angle in a road data picture */
  double RoadModel::findSteeringAngle(Mat &grayImage, int &straight, int &curved, int &noline) {
    const Point comp = Point(0, 0);
    Point l_pnt, r_pnt;
    int width, x;
    int height;
    if ((steeringAngle > 5 && !(steeringAngle < 0))
        || (steeringAngle < -5 && !(steeringAngle > 0))) {
      height = curved; // for simulator this value should be 325
    } else {
      height = straight; // for simulator this value should be 450
    }
    // look for valid points
    for (int y = grayImage.rows - 80; y > 300; y--) {
      // start algorithm for point search left and right
      l_pnt = pointSearchLeft(grayImage, y);
      r_pnt = pointSearchRight(grayImage, y);
      // the logic for how to calculate the angle
      if (l_pnt != comp && r_pnt != comp && r_pnt.x - l_pnt.x < 470) {
        // here starts the angle calculation, the formula is simply tan-1 = width / height
        x = (r_pnt.x - l_pnt.x) / 2 + l_pnt.x;
        width = x - 320;
        steeringAngle = atan2(width, height) * Constants::RAD2DEG;
        // since we found a valid steering angle, we set the old angle to the new value
        oldAngle = steeringAngle;
        // since we found two valid points we can set the roadSize
        if (y == 400) {
          roadMiddle = (r_pnt.x - l_pnt.x) / 2;
          cout << "roadMiddle: " << roadMiddle * 2 << "\n";
        }
        break;
      } else {
        // no two points where found, we set the steeringAngle to match the oldAngle.
        steeringAngle = oldAngle;
      }
    }

    // adding circles and lines for debugging purpose TODO remove these
    circle(grayImage, l_pnt, 3, 255, 3);
    circle(grayImage, r_pnt, 3, 255, 3);
    //	line(grayImage, Point(320, 480), Point(320, 0), 255, 3);
    //	imshow("debug", grayImage);

    // steering data logic calculations here
    // missing lines due to intersection
    if (r_pnt == comp && l_pnt == comp
        && (steeringAngle < 3 && steeringAngle > -3)) {
      oldAngle = 0;
      return oldAngle;
    }
    if (r_pnt == comp || l_pnt == comp) {
      r_pnt = pointSearchRight(grayImage, 400);
      l_pnt = pointSearchLeft(grayImage, 400);
      height = noline; // for simulator this value should be 260
    }
    // markings missing on left side
    if (l_pnt == comp && r_pnt != comp) {
      x = r_pnt.x - roadMiddle;
      width = x - 320;
      cout << "LEFT HAND TURN, W/O LINE MARKINGS\n";
      steeringAngle = atan2(width, height) * Constants::RAD2DEG;
      circle(grayImage, Point(x, 470), 3, 255, 3);
      return steeringAngle;
    }
    // markings missing on left side
    else if (l_pnt != comp && r_pnt == comp) {
      x = l_pnt.x + roadMiddle;
      width = x - 320;
      cout << "RIGHT HAND TURN, W/O LINE MARKINGS\n";
      steeringAngle = atan2(width, height) * Constants::RAD2DEG;
      circle(grayImage, Point(x, 470), 3, 255, 3);
      return steeringAngle;
    }
    /*
    // here we check if the line on the right side of the road is lost in the
    // middle of a corner, then we steer towards the right.
    else if (r_pnt == comp && steeringAngle > 1) {
    oldAngle = 16;
    return steeringAngle;
    }
    else if (l_pnt == comp && steeringAngle < 1) {
    oldAngle = -16;
    return steeringAngle;
    } */
    else {
      return steeringAngle;
    }
  }

  bool RoadModel::findIntersection(Mat &grayImage) {
    Point right = Point(0, 0);
    Point left = Point(0, 0);
    int value = INT_MIN;
    for (int y = 320; y > 300; y--) {
      const unsigned char *row = grayImage.ptr<unsigned char>(y);
      int x;
      // Right side
      for (x = 350; x < 370; x++) {
        if (row[x] > THRESH) {
          right.x = x;
          right.y = y;
          circle(grayImage, right, 3, 255);
          break;
        }
      }
      // Left side
      for (x = 250; x > 230; x--) {
        if (row[x] > THRESH) {
          left.x = x;
          left.y = y;
          circle(grayImage, left, 3, 255);
          break;
        }
      }
      line(grayImage, Point(320, 480), Point(320, 0), 255, 3);
      //imshow("debug", grayImage);
      if (left != Point(0, 0) && right != Point(0, 0)) {
        value = right.y - left.y;
      }
      if ((value < 5 && value > -5)
          && (steeringAngle > -2 && steeringAngle < 2)) {
        cout << "intersection found\n";
        return true;
      }
    }
    return false;
  }

  /* This function searches for a point within the Mat */
  Point RoadModel::pointSearchRight(Mat &image, int y) {
    // Char pointer to the selected row, needed to check its value
    const unsigned char *row = image.ptr<unsigned char>(y);
    // Search loop, here the value of the char pointer will be checked and made
    // sure it's value isn't larger then the threshold
    // point will then get the correct x and y value
    for (int x = (image.cols / 2) + 100; x < image.cols; x++) {
      if (row[x] > THRESH) {
        return Point(x, y);
      }
    }
    return Point(0, 0);
  }
  /* This function searches for a point within the Mat */
  Point RoadModel::pointSearchLeft(Mat &image, int y) {
    // Char pointer to the selected row, needed to check its value
    const unsigned char *row = image.ptr<unsigned char>(y);
    // Search loop, here the value of the char pointer will be checked and made
    // sure it's value isn't larger then the threshold
    // point will then get the correct x and y value
    for (int x = (image.cols / 2) - 100; x > 0; x--) {
      if (row[x] > THRESH) {
        return Point(x, y);
      }
    }
    return Point(0, 0);
  }

} // end msv

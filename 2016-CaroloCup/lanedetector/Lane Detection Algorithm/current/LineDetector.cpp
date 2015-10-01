//
// Created by parallels on 9/30/15.
//

#include "LineDetector.h"

LineDetector::LineDetector(Config m_config) {
    m_config.XTimesYMin = 2;
    m_config.XTimesYMax = 20;
    m_config.maxY = 235;
    m_config.maxArea = 4;
    m_config.debug = false;

}

void LineDetector::getContours() {
//    if(m_config.debug) {
//        startTimer = chrono::high_resolution_clock::now();
//    }
    findContours(image, contours, hierarchy, CV_RETR_TREE,
                 CV_CHAIN_APPROX_SIMPLE, Point(0, 0));
//    if(m_config.debug) {
//        startTimer = chrono::high_resolution_clock::now();
//    }
}

void LineDetector::getPolygonContours() {
    contours_poly.resize(contours.size());
    dashLines = solidLines = vector<CustomLine>(contours.size());
    for (unsigned int i = 0; i < contours.size(); i++) {
        approxPolyDP(Mat(contours[i]), contours_poly[i], 3, true);
    }
}

void LineDetector::getBoundingBoxes() {
    // RotatedRect declare in the LineDetector.h

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        rect = minAreaRect(contours_poly[i]);
        Point2f rect_points[4];
        rect.points(rect_points);
        int sizeX = 0, sizeY = 0, sizeR = 0;
        Point shortSideMiddle, longSideMiddle;
        // Find rect sizes
        for (int j = 0; j < 4; j++) {
            sizeR = (int) cv::sqrt(
                    cv::pow((rect_points[j].x - rect_points[(j + 1) % 4].x), 2)
                    + cv::pow(
                            (rect_points[j].y
                             - rect_points[(j + 1) % 4].y), 2));
            if (sizeX == 0) {
                sizeX = sizeR;
                shortSideMiddle.x = (int) ((rect_points[j].x
                                            + rect_points[(j + 1) % 4].x) / 2);
                shortSideMiddle.y = (int) ((rect_points[j].y
                                            + rect_points[(j + 1) % 4].y) / 2);
            }  else if (sizeY == 0 && sizeR != sizeX) {
                sizeY = sizeR;
                longSideMiddle.x = (int) ((rect_points[j].x
                                           + rect_points[(j + 1) % 4].x) / 2);
                longSideMiddle.y = (int) ((rect_points[j].y
                                           + rect_points[(j + 1) % 4].y) / 2);
            }
        }

        if (sizeX > sizeY) {
            Point2f temp;
            sizeR = sizeX;
            sizeX = sizeY;
            sizeY = sizeR;
            temp = longSideMiddle;
            longSideMiddle = shortSideMiddle;
            shortSideMiddle = temp;
        }

        rects.push_back(rect);
        PolySize polysize = {sizeX, sizeY, sizeR, shortSideMiddle, longSideMiddle};
        line_sizes.push_back(polysize);
    }
}
void LineDetector::classifyLines() {
    int sizeX;
    int sizeY;
    int sizeR;
    int area;
    Point2f rect_points[4];
    Point rectCenter;
    Point shortSideMiddle;
    for (unsigned int i = 0; i < line_sizes.size(); i++) {
        sizeX = line_sizes[i].sizeX;
        sizeY = line_sizes[i].sizeY;
        sizeR = line_sizes[i].sizeR;
        shortSideMiddle = line_sizes[i].shortSideMiddle;
        area = sizeX * sizeY;
        rect = rects[i];
        rect.points(rect_points);
        rectCenter.x = rect.center.x;
        rectCenter.y = rect.center.y;
        rect.angle = getLineSlope(shortSideMiddle, rectCenter);
        if (sizeY > m_config.XTimesYMin * sizeX
            && sizeY < m_config.XTimesYMax * sizeX
            && sizeY < m_config.maxY) {
            dashLines[cntDash] = createLineFromRect(&rect, sizeX, sizeY, i);
            cntDash++;
        }
        else if (sizeY > sizeX && sizeY > (m_config.maxY / 2)
                 && area < m_config.maxArea * 10000) {
            solidLines[cntSolid] = createLineFromRect(&rect, sizeX, sizeY, i);
            cntSolid++;
        } else if (area > m_config.maxArea * 10000) {
            minXI = w;
            minYI = h;
            for (int j = 0; j < 4; j++) {
                if (minXI > rect_points[j].x) {
                    minXI = rect_points[j].x;
                }
                if (minYI > rect_points[j].y) {
                    minYI = rect_points[j].y;
                }
            }
            YI = rectCenter.y;
            intersectionRect = i;
            bigRect = rects[i];

            //intersectionOn = true;
            //foundIntersection = true;
            float angle_thr = 10;
            float height_thr = (1 * h) / 2;
            if ((abs(rect.angle) < angle_thr || 180 - abs(rect.angle) < angle_thr) && rectCenter.y > height_thr &&
                roadState == NORMAL) {
                roadState = INTERSECTION;
                //confidenceLevel = CONFIDENCE_LEVEL_MAX;
                intersectionOn = true;
                calcIntersectionGoalLine = true;
                foundIntersection = true;

//                TimeStamp currentTime;
//                intersection_start = currentTime.toMicroseconds();
                auto intersection_start = chrono::high_resolution_clock::now();
            }
        }
    }

    auto endTime = chrono::high_resolution_clock::now();
    long time_taken_contour = chrono::duration_cast<chrono::microseconds>(endTime - intersection_start).count();
    if (time_taken_contour > 800) {
        roadState = NORMAL;
        intersectionOn = false;
        foundIntersection = false;
        calcIntersectionGoalLine = false;
    }
    if (intersectionRect == -1 && roadState == INTERSECTION) {
        cout << "STOPS updating intersection_goalLine: " << endl;
        calcIntersectionGoalLine = false;
    }

    if (intersectionOn && !foundIntersection) {
        YI = h;
    }
}

void LineDetector::filterAndMerge() {
    for (int j = 0; j < cntSolid; j++) {
        float a = tan(M_PI * solidLines[j].slope / 180);
        Point center;
        center.x = (solidLines[j].p1.x + solidLines[j].p2.x) / 2;
        center.y = (solidLines[j].p1.y + solidLines[j].p2.y) / 2;
        float b = center.y - center.x * a;
        if ((solidLines[j].slope > MIN_ANGLE - 5
             && max(solidLines[j].p1.x, solidLines[j].p2.x) > w / 2)
            || (solidLines[j].slope < (-1) * (MIN_ANGLE - 5)
                && min(solidLines[j].p1.x, solidLines[j].p2.x) < w / 2)) {
            for (int l = 0; l < cntDash; l++) {
                Point dashCenter;
                dashCenter.x = (dashLines[l].p1.x + dashLines[l].p2.x) / 2;
                dashCenter.y = (dashLines[l].p1.y + dashLines[l].p2.y) / 2;
                float res = a * dashCenter.x + b;
                if (res > dashCenter.y) {
                    dashLines[l] = dashLines[cntDash - 1];
                    cntDash--;
                    l--;
                }
            }
            for (int k = j + 1; k < cntSolid; k++) {
                Point sldCenter;
                sldCenter.x = (solidLines[k].p1.x + solidLines[k].p2.x) / 2;
                sldCenter.y = (solidLines[k].p1.y + solidLines[k].p2.y) / 2;
                float res = a * sldCenter.x + b;
                if (res > sldCenter.y) {
                    solidLines[k] = solidLines[cntSolid - 1];
                    cntSolid--;
                    k--;
                }
            }
        }
    }
}

void LineDetector::finalFilter() {
    for (int i = 0; i < cntSolid; i++) {
        CustomLine l = solidLines[i];
        int minAngle = MIN_ANGLE - 5;
        if (abs(l.slope) < minAngle) {
            solidLines[i] = solidLines[cntSolid - 1];
            cntSolid--;
            if (i > 0) {
                i--;
            }
            foundStopStartLine = true;
        }
    }
    //Dash also positioned too high on the image or too left or too right
    int maxDashY = 0;
    for (int i = 0; i < cntDash; i++) {
        CustomLine l = dashLines[i];
        int dashCenterX = (l.p1.x + l.p2.x) / 2;
        int dashCenterY = (l.p1.y + l.p2.y) / 2;
        if ((l.slope < MIN_ANGLE) && (l.slope > ((-1) * MIN_ANGLE))
            || (dashCenterY < h / 15) || (dashCenterX > 19 * w / 20)) //|| (dashCenterX < w/20) too left //too high
        {
            dashLines[i] = dashLines[cntDash - 1];
            cntDash--;
            if (i > 0) {
                i--;
            }
        }

        if (maxDashY < max(dashLines[i].p1.y, dashLines[i].p2.y)) {
            maxDashY = max(dashLines[i].p1.y, dashLines[i].p2.y);
        }
    }
}

float getLineSlope(Point &p1, Point &p2) {
    float slope = M_PI / 2;
    if ((p1.x - p2.x) != 0) {
        slope = (p1.y - p2.y) / ((float) (p1.x - p2.x));
        slope = atan(slope);
    }
    if (slope < 0) {
        return 180 + (slope * 180 / M_PI);
    }
    return slope * 180 / M_PI;
}

CustomLine createLineFromRect(RotatedRect *rect, int sizeX, int sizeY, int polygonIndex) {
    Point2f rect_points[4];
    rect->points(rect_points);

    CustomLine l;
    Point pt1, pt2;
    l.polygonIndex = polygonIndex;
    if (rect->angle < 90) {
        float angle = rect->angle * M_PI / 180;
        float xOffset = cos(angle) * sizeY / 2;
        float yOffset = sin(angle) * sizeY / 2;
        pt1.y = rect->center.y + yOffset;
        pt1.x = rect->center.x + xOffset;
        pt2.y = rect->center.y - yOffset;
        pt2.x = rect->center.x - xOffset;
    }
    else {
        rect->angle = rect->angle - 180;
        float angle = (-rect->angle) * M_PI / 180;
        float xOffset = cos(angle) * sizeY / 2;
        float yOffset = sin(angle) * sizeY / 2;
        pt1.y = rect->center.y + yOffset;
        pt1.x = rect->center.x - xOffset;
        pt2.y = rect->center.y - yOffset;
        pt2.x = rect->center.x + xOffset;
    }
    l.p1 = pt1;
    l.p2 = pt2;
    l.slope = rect->angle;
    return l;
}
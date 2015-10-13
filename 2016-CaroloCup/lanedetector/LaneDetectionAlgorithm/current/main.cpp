#include "main.h"

bool showcase = false;
auto startTimer = chrono::high_resolution_clock::now();
auto endTimer = chrono::high_resolution_clock::now();
double periodTimer = 0;
double totalTimer = 0;

int main(int argc, char **argv) {
    m_config.XTimesYMin = 2;
    m_config.XTimesYMax = 20;
    m_config.maxY = 235;
    m_config.maxArea = 4;

    // Read supplied image path
    char *imageName = argv[1];
    if (!readImage(imageName, argc)) {
        printf(" No image data \n ");
        return -1;
    }
    if (showcase) imshow("Original Image", image);

    toGrayScale();
    if (showcase) imshow("Gray Scale Image", image);

    cropImage();
    if (showcase) imshow("Cropped Image", image);

    // Set frame size for future calculations
    w = image.size().width;
    h = image.size().height;

    // Make a copy of the current image for demonstration purposes
    if (showcase) {
        image.copyTo(originalImage);
        cvtColor(originalImage, originalImage, CV_GRAY2BGR);
    }

    applyThreshold();
    if (showcase) imshow("Applied Threshold", image);

    getContours();
    if (showcase) displayContours();

    getPolygonContours(); // Also part of getContours()
    if (showcase) displayPolygonContours();

    getBoundingBoxes(); // Also, getRectangles()
    if (showcase) displayBoundingBoxes();

    classifyLines();
    if (showcase) {
        displayDashedLines();
        displaySolidLines();
    }

    filterAndMerge();
    if (showcase) displayBothLines("Filter and Merge");

    finalFilter();
    if (showcase) displayBothLines("Final Filter");

    characteristicFiltering(&ltu);
    if (showcase) displaySelectedLines();

    createTrajectory(&ltu);
    if (showcase) displayTrajectory();

    createIntersectionGoalLine();

    cout << "Total: " << totalTimer << endl;

    waitKey(0);

    return 0;
}

int readImage(char *imageName, int argc) {
    image = imread(imageName, 1);
    if (argc != 2 || !image.data) return 0;
    return 1;
}

/***
 * START ALGORITHM STEPS
 */
void toGrayScale() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    cvtColor(image, image, CV_BGR2GRAY);

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "To Gray Scale: " << periodTimer << endl;
//TIMER
}

void cropImage() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    int height = image.size().height;
    int width = image.size().width;
    image = image(cv::Rect(1, 2 * height / 16 - 1, width - 1, 10 * height / 16 - 1));

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Crop Image: " << periodTimer << endl;
//TIMER
}

void applyThreshold() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    threshold(image, image, getDynamicThresh(-2), 255, CV_THRESH_BINARY);

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Applied Threshold: " << periodTimer << endl;
//TIMER
}

void getContours() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    cntDash = 0;
    cntSolid = 0;

    findContours(image, contours, hierarchy, CV_RETR_TREE,
                 CV_CHAIN_APPROX_SIMPLE, Point(0, 0));

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Get Contours: " << periodTimer << endl;
//TIMER
}

void getPolygonContours() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    contours_poly.resize(contours.size());

    dashLines = vector<CustomLine>(contours.size());
    solidLines = vector<CustomLine>(contours.size());

    for (unsigned int i = 0; i < contours.size(); i++) {
        approxPolyDP(Mat(contours[i]), contours_poly[i], 3, true);
    }

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Get Polygon Contours: " << periodTimer << endl;
//TIMER
}

void getBoundingBoxes() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    RotatedRect rect;

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        rect = minAreaRect(contours_poly[i]);
        Point2f rect_points[4];
        rect.points(rect_points);
        int sizeX = 0, sizeY = 0, sizeR = 0;
        Point shortSideMiddle;
        Point longSideMiddle;
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

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Get Bounding Boxes: " << periodTimer << endl;
//TIMER
}

void classifyLines() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    //confidenceLevel = 0;
    int sizeX;
    int sizeY;
    int sizeR;
    int area;
    RotatedRect rect;
    Point2f rect_points[4];
    Point rectCenter;
    Point shortSideMiddle;
    //intersectionRect = -1;
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

//        TimeStamp endTime;
    auto endTime = chrono::high_resolution_clock::now();
//        long time_taken_contour = (endTime.toMicroseconds() - intersection_start)/ 1000.0;
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

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Classify Lines: " << periodTimer << endl;
//TIMER
}

void filterAndMerge() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

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

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Filter and Merge: " << periodTimer << endl;
//TIMER
}

void finalFilter() {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

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

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Final Filter: " << periodTimer << endl;
//TIMER
}

void characteristicFiltering(LinesToUse *ltu) {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    if (roadState == INTERSECTION) {
        // reset x-values
        currentDashGoalX = 0;
        currentRightGoalX = 0;
        currentLeftGoalX = 0;
    }

    ltu->foundR = false;
    ltu->foundL = false;
    ltu->foundD = false; // shrinkSize=false; Avoid unused variable warning

    ltu->dashLineVec = Vec4i(0, 0, 0, 0);
    ltu->leftLineVec = Vec4i(0, 0, 0, 0);
    ltu->rightLineVec = Vec4i(0, 0, 0, 0);
    //Pick the suitable dashLine
    ltu->dashedCurveFound = false;
    if (cntDash > 0) {
        ltu->dashLine.p1.y = 0;
        ltu->dashLine.p2.y = 0;
        // Sort the dashes by highest p1.y value first
        std::sort(dashLines.begin(), dashLines.begin() + cntDash);
        ltu->cntDash = cntDash;
        // Make p1 be the bottom point
        for (int i = 0; i < cntDash; i++) {
            if (dashLines[i].p1.y < dashLines[i].p2.y) {
                // Flipping p1 <-> p2, recalc slope.
                Point tmp = dashLines[i].p1;
                dashLines[i].p2 = dashLines[i].p1;
                dashLines[i].p2 = tmp;
                dashLines[i].slope = getLineSlope(dashLines[i].p1, dashLines[i].p2);
            }
        }
        // Try to find the dashed curve composed of several dashes
        //
        // TODO: it now assumes that the dashed curves are completely independent;
        // that a curve can not split into more curves. Right now it just picks the
        // first dash that matches and append that to the found curve, it does not
        // continue to look if other dashes also matches. (findCurve should spawn
        // one CustomLine vector for eash found curve)
        //
        if (cntDash > 1) {
            // Copy needed data
            std::vector<CustomLine> dashedLines = dashLines;
            std::vector<CustomLine> unusedLines;
            int cntDashed = cntDash;

            // The found curves will be stored in this
            std::vector<vector<CustomLine> > curves;

            for (int i = 0; i < cntDash - 1; i++) {
                // When fiding curve from i, dashes present at i < don't need to be checked.
                std::vector<CustomLine> lines;
                for (int j = 0; j < cntDashed; j++) {
                    lines.push_back(dashedLines[j]);
                }

                std::vector<CustomLine> res = findCurve(lines);
                if (res.size() == 0) {
                    unusedLines.push_back(dashedLines[0]);
                    dashedLines.erase(dashedLines.begin());
                    cntDashed--;
                    continue;
                }
                // If curve found, we proceed
                std::vector<CustomLine> curve;
                curve.push_back(lines[0]);

                for (int j = 0; j < res.size(); j++) {
                    curve.push_back(res[j]);
                }
                // Save the found curve
                curves.push_back(curve);

                // Remove dashed lines already a part of a found curve
                for (int j = 0; j < curve.size(); j++) {
                    for (int k = 0; k < cntDashed; k++) {
                        if (curve[j] == dashedLines[k]) {
                            dashedLines.erase(dashedLines.begin() + k);
                            cntDashed--;
                        }
                    }
                }
                // Check if we got enough lines to make another curve
                if (cntDashed < 2) {
                    break;
                }
            }
            // Pick which curve to use
            int maxY = 0;
            for (int i = 0; i < curves.size(); i++) {
                int dashSupPosX = getIntersectionWithBottom(curves[i][0]);

                if (curves[i][0].p1.y > maxY &&
                    (abs(dashSupPosX - currentDashGoalX) < calcRoadSize * 0.8
                     || currentDashGoalX == 0)) {
                    dashLines[0] = curves[i][0];
                    ltu->dashedCurve = curves[i];
                    ltu->dashedCurveFound = true;
                    maxY = curves[i][0].p1.y;

                }
            }
//            global_dashedCurve = ltu->dashedCurve;
            // Check if any remaining dashed lines not a part of a curve could be potential
            // left or right lines.
            if (ltu->dashedCurveFound) {
                for (int j = 0; j < cntDashed; j++) {
                    unusedLines.push_back(dashedLines[j]);
                }

                for (int i = 0; i < unusedLines.size(); i++) {
                    int s = ltu->dashedCurve.size() - 1;

                    // Checks if a unused dash line is interleaved with the found dashed curve w.r.t the y-axis
                    if (!(((unusedLines[i].p1.y > ltu->dashedCurve[0].p1.y) &&
                           (unusedLines[i].p2.y > ltu->dashedCurve[0].p1.y)) ||
                          ((unusedLines[i].p1.y < ltu->dashedCurve[s].p2.y) &&
                           (unusedLines[i].p2.y < ltu->dashedCurve[s].p2.y)))) {
                        // Filter away short lines that probably do not have an accurate angle
                        if (getDist(unusedLines[i].p1, unusedLines[i].p2) > (m_config.maxY / 3)) {
                            solidLines[cntSolid] = unusedLines[i];
                            cntSolid++;
                        } else {
                        }
                    }
                }
                // set currentDashGoalX
                ltu->dashLine = ltu->dashedCurve[0];
                currentDashGoalX = getIntersectionWithBottom(ltu->dashLine);
                ltu->dashLineVec = Vec4i(ltu->dashLine.p1.x, ltu->dashLine.p1.y, ltu->dashLine.p2.x,
                                         ltu->dashLine.p2.y);
                ltu->foundD = true;
            }
        }

        //
        // The old way to do it follows
        //
        if (!ltu->dashedCurveFound) {
            for (int i = 0; i < cntDash; i++) {
                for (int j = 0; j < cntDash; j++) {
                }
                if (i != cntDash - 1 && max(dashLines[i + 1].p1.y, dashLines[i + 1].p2.y)
                                        > min(dashLines[i].p1.y, dashLines[i].p2.y)) {
                    int positionX = getIntersectionWithBottom(dashLines[i]);
                    int nPositionX = getIntersectionWithBottom(dashLines[i + 1]);
                    if (abs(currentDashGoalX - positionX)
                        < abs(currentDashGoalX - nPositionX)) {
                        dashLines.erase(dashLines.begin() + 1);
                        cntDash--;
                    }
                    else {
                        dashLines.erase(dashLines.begin());
                        cntDash--;
                    }
                    if (i > 0) {
                        i--;
                    }
                }

                if (intersectionOn && ((dashLines[i].p1.y + dashLines[i].p2.y) / 2) < YI) {
                    dashLines[i] = dashLines[cntDash - 1];
                    cntDash--;
                    if (i > 0) {
                        i--;
                    }
                }
            }
            if (cntDash > 0) {
                ltu->dashLine = dashLines[0];
                int dashSupPosX = getIntersectionWithBottom(ltu->dashLine);
                //if(ltu->dashLine.slope < 0) {
                if (abs(dashSupPosX - currentDashGoalX) < calcRoadSize * 0.8
                    || currentDashGoalX == 0) {
                    /*if(max(ltu->dashLine.p1.x, ltu->dashLine.p2.x) < w/10) {
                     shrinkSize = true;
                     }*/
                    ltu->dashLineVec = Vec4i(ltu->dashLine.p1.x, ltu->dashLine.p1.y,
                                             ltu->dashLine.p2.x, ltu->dashLine.p2.y);
                    ltu->foundD = true;
                    currentDashGoalX = dashSupPosX;
                }
            }
            ltu->dashedCurve.push_back(dashLines[0]);

        }
    }

    // Determine which solid line is the left and right solid lines
    if (cntSolid > 0 && !intersectionOn) {
        ltu->rightLine.p1.x = w + 50;
        ltu->rightLine.p2.x = w + 50;
        for (int i = 0; i < cntSolid; i++) {
            if (solidLines[i].slope < 90 && solidLines[i].slope > 0
                && min(solidLines[i].p1.x, solidLines[i].p2.x)
                   < min(ltu->rightLine.p1.x, ltu->rightLine.p2.x)) {
                ltu->rightLine = solidLines[i];
                ltu->foundR = true;
            }
        }
        if (ltu->foundR) {
            int rSupPosX = getIntersectionWithBottom(ltu->rightLine);

            if (abs(rSupPosX - currentRightGoalX) < calcRoadSize * 0.8
                || currentRightGoalX == 0) {
                ltu->rightLineVec = Vec4i(ltu->rightLine.p1.x, ltu->rightLine.p1.y,
                                          ltu->rightLine.p2.x, ltu->rightLine.p2.y);
                currentRightGoalX = rSupPosX;
            }
            else {
                ltu->foundR = false;
            }
        }

        ltu->leftLine.p1.x = -50;
        ltu->leftLine.p2.x = -50;
        for (int i = 0; i < cntSolid; i++) {
            // centerSolidLineX commented to avoid unused variable warning!
            //int centerSolidLineX = (solidLines[i].p1.x + solidLines[i].p2.x)/2;
            if (solidLines[i].slope > -90 && solidLines[i].slope < 0
                && min(solidLines[i].p1.x, solidLines[i].p2.x)
                   > min(ltu->leftLine.p1.x, ltu->leftLine.p2.x)) {
                ltu->leftLine = solidLines[i];
                ltu->foundL = true;
            }
        }
        if (ltu->foundL) {
            int lSupPosX = getIntersectionWithBottom(ltu->leftLine);
            if (abs(lSupPosX - currentLeftGoalX) < calcRoadSize * 0.8
                || currentLeftGoalX == 0) {
                ltu->leftLineVec = Vec4i(ltu->leftLine.p1.x, ltu->leftLine.p1.y,
                                         ltu->leftLine.p2.x, ltu->leftLine.p2.y);
                currentLeftGoalX = lSupPosX;
            }
            else {
                ltu->foundL = false;
            }
        }
    }

    //////////////////
    // This check is used to find out if we have passed an intersection.
    // If it is passed, it will set the roadState to NORMAL.
    //////////////////
    if (roadState == INTERSECTION) {
        int minSlope = 40;

        int maxY = 0;

        if (ltu->dashedCurve.size() > 1) {
            for (int i = 0; i < ltu->dashedCurve.size(); i++) {
                if (maxY < ltu->dashedCurve[i].p1.y) { // p1 is always the point furtest down on the frame
                    maxY = ltu->dashedCurve[i].p1.y;
                }
            }

            if (abs(ltu->dashedCurve[0].slope) > minSlope && maxY > (8 * h / 10)) {

                roadState = NORMAL;
                intersectionOn = false;
                foundIntersection = false;
                calcIntersectionGoalLine = false;
            }
        } else if (ltu->foundR) {
            maxY = ltu->rightLine.p1.y;
            if (abs(ltu->rightLine.slope) > minSlope && maxY > (8 * h / 10)) {
                roadState = NORMAL;
                intersectionOn = false;
                foundIntersection = false;
                calcIntersectionGoalLine = false;

            }
        } else if (ltu->foundL) {
            maxY = ltu->leftLine.p1.y;
            if (abs(ltu->leftLine.slope) > minSlope && maxY > (8 * h / 10)) {
                roadState = NORMAL;
                intersectionOn = false;
                foundIntersection = false;
                calcIntersectionGoalLine = false;

            }
        }
    }

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Select Lines: " << periodTimer << endl;
//TIMER
}

void createTrajectory(LinesToUse *ltu) {
//TIMER
    startTimer = chrono::high_resolution_clock::now();
//TIMER

    // The found lines are used to create a trajectory for the car's future movement

    if (!(ltu->foundL || ltu->foundD || ltu->foundR) || roadState == INTERSECTION) {
        cout << "No lines found or INTERSECTION mode, trajectory will not be derived." << endl;

//        finalOutput.noTrajectory = true;
//        finalOutput.intersection_goalLine = false;
        dataToDriver = new LaneDetectorDataToDriver(); // Empty call will set noTrajectory = true
        return;
    }
    std::vector<int> defaultCutPoints;
    defaultCutPoints.push_back(180);
    defaultCutPoints.push_back(100);
    defaultCutPoints.push_back(50);
    std::vector<int> cutPoints;
    std::vector<CustomLine> leftSplitted;
    std::vector<CustomLine> rightSplitted;
    std::vector<CustomLine> dashToUse;

    //////////////////
    // This code is used for the simplest option possible and will not cut any solid line
    // and it will provide maximum one goalLine
    //////////////////
    if (ltu->foundR)
        rightSplitted.push_back(ltu->rightLine);
    else
        rightSplitted.push_back(getNoneCustomLine());

    if (ltu->foundL)
        leftSplitted.push_back(ltu->leftLine);
    else
        leftSplitted.push_back(getNoneCustomLine());

    if (ltu->foundD)
        dashToUse.push_back((ltu->dashedCurve)[0]);
    else
        dashToUse.push_back(getNoneCustomLine());
    cutPoints.push_back(h);
    //////////////////
    // End simplest option code
    //////////////////

    //////////////////
    // This for loop creates the goalLines, one for each iteration.
    // The three lines (left, dash, right) associated to a specific cut is fed to provideGoalLine(..).
    // Remember that those can potentially be dummy lines.
    //////////////////
    std::vector<CustomLine> rightGoalLines, leftGoalLines;
    std::vector<int> confidenceLevel_goalLine;
    for (int i = 0; i < dashToUse.size(); i++) {
        EstimationData ed;
        GoalLineData gld;
        ed.left = leftSplitted[i];
        ed.dash = dashToUse[i];
        ed.right = rightSplitted[i];
        if (i == 0)
            ed.yPosition = (2 * h) / 3.;
        else
            ed.yPosition = cutPoints[i - 1];

        provideGoalLine(&ed, &gld);
        confidenceLevel_goalLine.push_back(gld.confidenceLevel_rightGoalLine);
        rightGoalLines.push_back(gld.rightGoalLine);
        leftGoalLines.push_back(gld.leftGoalLine);
    }
    int confidenceLevel_goalLine0 = 0;
    if (rightGoalLines.size() > 0) {
        confidenceLevel_goalLine0 = confidenceLevel_goalLine[0];
    }

    //////////////////
    // Here is the currentLine derived, which states the car's current position and heading.
    //////////////////
    Point position;
    position.x = w / 2;
    position.y = h;
    Point heading;
    heading.x = w / 2;
    heading.y = 0;
    //Create car orientation vector
    CustomLine currentLine;
    currentLine.p1 = heading;
    currentLine.p2 = position;
    currentLine.slope = getLineSlope(heading, position);


    //////////////////
    // Create a object that the laneDetector can send to driver.
    //////////////////

    dataToDriver = new LaneDetectorDataToDriver(leftGoalLines[0], rightGoalLines[0], currentLine, false,
                                                confidenceLevel_goalLine0);

//TIMER
    endTimer = chrono::high_resolution_clock::now();
    periodTimer = chrono::duration_cast<chrono::microseconds>(endTimer - startTimer).count();
    periodTimer = periodTimer / 1000;
    totalTimer += periodTimer;
    cout << "Create Trajectory: " << periodTimer << endl;
//TIMER
}

void createIntersectionGoalLine(){
    CustomLine newRightGoalLine = getNoneCustomLine();
    CustomLine newLeftGoalLine = getNoneCustomLine();
    if (roadState == INTERSECTION){
        dataToDriver->leftGoalLines0 = getNoneCustomLine();
        dataToDriver->rightGoalLines0 = getNoneCustomLine();

        if (intersectionRect != -1 && calcIntersectionGoalLine == true){
            Point rectCenter;
            Point2f rect_points[4];
            bigRect.points(rect_points);
            PolySize p = createPolySize(bigRect);
            // Calc rect width
            int rect_width = 0;
            for (int i = 0; i < 3; i++){
                int now_width = abs(rect_points[i].x - rect_points[i+1].x);
                if (now_width > rect_width)
                    rect_width = now_width;
            }
            // Calc if rect intersects current line
            int xCut = w/2.;
            bool rectXBigger = false, rectXSmaller = false;
            Point dashPoint;
            int dashXPos = w;
            for (int i = 0; i < 4; i++){
                if (rect_points[i].x < dashXPos){
                    dashXPos = rect_points[i].x;
                    dashPoint = rect_points[i];
                }

                if(rect_points[i].x < xCut)
                    rectXSmaller = true;
                else if (rect_points[i].x > xCut)
                    rectXBigger = true;
            }

            if (rectXBigger && rectXSmaller && rect_width < 0.95*w && YI > h/2){
                rectCenter.x = bigRect.center.x;
                rectCenter.y = bigRect.center.y;
                float rectSlope = getLineSlope(p.longSideMiddle, rectCenter);
                newRightGoalLine.p1 = dashPoint;
                newRightGoalLine.p1.x += ROAD_SIZE/2;
                newRightGoalLine.slope = rectSlope - 2*(rectSlope - 90);
                newRightGoalLine.p2.x = getIntersectionWithBottom(newRightGoalLine);
                newRightGoalLine.p2.y = h;
                newRightGoalLine.p1.x = getIntersectionWithTop(newRightGoalLine);
                newRightGoalLine.p1.y = 0;
                // Derive left goal line
                newLeftGoalLine = newRightGoalLine;
                newLeftGoalLine.slope = abs(newRightGoalLine.slope) + ROAD_ANGLE*0.65;// 2*(90 - ROAD_ANGLE);//180 - abs(newRightGoalLine.slope) - ROAD_ANGLE;
                newLeftGoalLine.p2.x = newRightGoalLine.p2.x - ROAD_SIZE;
                newLeftGoalLine.p1.x = getIntersectionWithTopP2(newLeftGoalLine);
            }
        }
        Point position;
        position.x = w / 2;
        position.y = h;
        Point heading;
        heading.x = w / 2;
        heading.y = 0;
        //Create car orientation vector
        CustomLine currentLine;
        currentLine.p1 = heading;
        currentLine.p2 = position;
        currentLine.slope = getLineSlope(heading, position);

        if(isNoneCustomLine(newRightGoalLine)){
            dataToDriver->noTrajectory = true;
        }else{
            dataToDriver->noTrajectory = false;
            dataToDriver->rightGoalLines0 = newRightGoalLine;
            dataToDriver->leftGoalLines0 = newLeftGoalLine;
            dataToDriver->currentLine = currentLine;
        }
    }
}
/***
 * STOP ALGORITHM STEPS
 */

/***
 * START ALGORITHM HELPER FUNCTIONS
 */
int getDynamicThresh(int lux) {
    int baseThresh = 48;
    int minIntervalValue[] = {11, 15, 17, 20, 23, 26, 29, 32}, maxIntervalValue[] = {16, 18, 21, 24, 27, 31, 35, 40};
    int foundIndex[3], thresh[] = {baseThresh + 2, baseThresh + 7, baseThresh + 12, baseThresh + 17, baseThresh + 22,
                                   baseThresh + 27, baseThresh + 32};
    if (lux < minIntervalValue[0]) {
        return baseThresh;
    }
    if (lux > maxIntervalValue[6]) {
        return baseThresh + 42;
    }
    int cnt = 0;
    for (int i = 0; i < 7; i++) {
        if (lux >= minIntervalValue[i] && lux <= maxIntervalValue[i]) {
            foundIndex[cnt++] = i;
        }
    }
    for (int j = 0; j < cnt; j++) {
        if (previousThresh == thresh[foundIndex[j]]) {
            return thresh[foundIndex[j]];
        }
    }
    return thresh[foundIndex[0]];
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

int getIntersectionWithBottom(CustomLine l) {
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if (abs(a) > 0.001) {
        positionX = (h - b) / a;
    }
    return positionX;
}

vector<CustomLine> findCurve(vector<CustomLine> lines) {
    // used to be sure that you have found the dashes.
    std::vector<CustomLine> curve;
    if (lines.size() < 2) {
        return curve;
    }
    for (int j = 1; j < lines.size(); j++) {
        // The snd dash has to be above the fst one
        if (lines[0].p2.y > lines[j].p1.y) {
            // The slope diviation between the dashes is limited
            float slopeA, slopeB;
            if (lines[0].slope < 0)
                slopeA = lines[0].slope + 180;
            else
                slopeA = lines[0].slope;

            if (lines[j].slope < 0)
                slopeB = lines[j].slope + 180;
            else
                slopeB = lines[j].slope;

            // The slope of a line drawn between the real lines
            float slopeInBetween = getLineSlope(lines[0].p2, lines[j].p1);

            if (slopeInBetween < 0)
                slopeInBetween += 180;

            // The slope deviation between the lines
            float slopeDiffLines = abs(slopeA - slopeB);
            float slopeDiffToLine0 = abs(slopeInBetween - slopeA);
            float slopeDiffToLineJ = abs(slopeInBetween - slopeB);
            //float distInBetween = getDist(lines[0].p2, lines[j].p1);


            if ((slopeDiffLines < 60) && (slopeDiffToLine0 < slopeDiffLines + 20) &&
                (slopeDiffToLineJ < slopeDiffLines + 20))// && (distInBetween < m_config.maxY * 0.7))
            {

                curve.push_back(lines[j]);

                if (lines.size() > 2) {
                    // Call recursively, but take away the already processed line
                    lines[0] = lines[j];
                    lines.erase(lines.begin() + j);
                    std::vector<CustomLine> res = findCurve(lines);

                    // Add the recursive result
                    for (int j = 0; j < res.size(); j++) {
                        curve.push_back(res[j]);
                    }
                    return curve;
                }
                else {
                    return curve;
                }
            }
        }
    }
    return curve;
}

float getDist(const Point p1, const Point p2) {
    return sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2));
}

CustomLine getNoneCustomLine() {
    CustomLine none;
    none.p1.x = 0;
    none.p1.y = 0;
    none.p2.x = 0;
    none.p2.y = 0;
    return none;
}

void provideGoalLine(EstimationData *ed, GoalLineData *gld) {
    bool printouts = false;

    int calcRoadAngle = 0, pureRoadSize;
    ed->calcRoadSize = 0;
    ed->isLeftEstimated = false;
    ed->isDashEstimated = false;
    ed->isRightEstimated = false;
    ed->foundGoal = false;
    gld->confidenceLevel_rightGoalLine = 0;
    float roadSizeAdjustment = 1 - (300 - (float(ed->yPosition))) / 350;

    CustomLine lineUsedForEstimation = getNoneCustomLine(); // used for debug of getRoadSize and getRoadAngle
    int whichLine = -1;

    bool foundL = !isNoneCustomLine(ed->left);
    bool foundD = !isNoneCustomLine(ed->dash);
    bool foundR = !isNoneCustomLine(ed->right);

    if (printouts) {
        cout << "__start provideGoalLine" << endl;
        cout << "ed->yPosition: " << ed->yPosition << endl;
        cout << "roadSizeAdjustment: " << roadSizeAdjustment << endl;
        cout << "foundL: " << foundL << " foundD: " << foundD << " foundR: " << foundR << endl;
    }



    //yPosition used to get the right roadwidth

    // If we got two lines, we do not need to estimate any lines
    if (foundD) {
        lineUsedForEstimation = ed->dash; // used for debug of getRoadSize and getRoadAngle
        whichLine = 1; // used for debug of getRoadSize and getRoadAngle
        calcRoadAngle = getRoadAngle(2, ed->dash.slope);
        ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
        if (printouts)
            cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;

        if (foundL && foundR) {
            // Calculate both goal lines
            gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
            gld->confidenceLevel_rightGoalLine = 5;
            gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->dash, ed);
        }
        else if (foundR) {
            // Calculate right goal line
            gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
            gld->confidenceLevel_rightGoalLine = 5;
            // Shift calculation result to get left goal line
            gld->leftGoalLine = gld->rightGoalLine;
            gld->leftGoalLine.p2.x -= ed->calcRoadSize;
            gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);
        }
        else if (foundL) {
            // Calculate left goal line
            gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->dash, ed);
            // Shift calculation result to get right goal line
            // OLD WAY
            gld->rightGoalLine = gld->leftGoalLine;
            gld->rightGoalLine.p2.x += ed->calcRoadSize;
            gld->rightGoalLine.slope = getLineSlope(gld->rightGoalLine.p2, gld->rightGoalLine.p1);
            gld->confidenceLevel_rightGoalLine = 4;
            // NEW WAY
            // int expectedRightLineX = getIntersectionWithY(ed->dash, ed->yPosition) + ed->calcRoadSize;
            // float expectedRightLineAngle = 180 - abs(ed->dash.slope)
            //                                - calcRoadAngle;
            // if (printouts)
            //     {
            //         cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
            //         cout << "calcRoadAngle: " << calcRoadAngle << endl;
            //         cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
            //         cout << "abs(ed->dash.slope): " << abs(ed->dash.slope) << endl;
            //     }

            // if (expectedRightLineAngle > 90)
            //     {
            //         expectedRightLineAngle = expectedRightLineAngle - 180;
            //     }
            // if (printouts)
            //     cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
            // ed->right.slope = expectedRightLineAngle;
            // ed->right.p1.x = expectedRightLineX;
            // ed->right.p1.y = ed->yPosition;
            // ed->isRightEstimated = true;

            // // Calculate right goal line
            // gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
            // gld->confidenceLevel_rightGoalLine = 3;
        }
        else {
            // Only dash found
            // Estimate either left line and/or right line.
            // Here I choose to estimate right line
            if (printouts)
                cout << "Found only dash, estimating right" << endl;
            int expectedRightLineX = getIntersectionWithY(ed->dash, ed->yPosition) + ed->calcRoadSize;
            float expectedRightLineAngle = 180 - abs(ed->dash.slope)
                                           - calcRoadAngle;
            if (printouts) {
                cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
                cout << "calcRoadAngle: " << calcRoadAngle << endl;
                cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
                cout << "abs(ed->dash.slope): " << abs(ed->dash.slope) << endl;
            }

            if (expectedRightLineAngle > 90) {
                expectedRightLineAngle = expectedRightLineAngle - 180;
            }
            if (printouts)
                cout << "expectedRightLineAngle: " << expectedRightLineAngle << endl;
            ed->right.slope = expectedRightLineAngle;
            ed->right.p1.x = expectedRightLineX;
            ed->right.p1.y = ed->yPosition;
            ed->isRightEstimated = true;

            // Calculate right goal line
            gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
            gld->confidenceLevel_rightGoalLine = 3;
            // Shift calculation result to get left goal line
            gld->leftGoalLine = gld->rightGoalLine;
            gld->leftGoalLine.p2.x -= ed->calcRoadSize;
            gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);
        }
    }
    else if (foundL && foundR) {
        if (printouts)
            cout << "Found right and left" << endl;
        ed->calcRoadSize =
                getIntersectionWithY(ed->right, ed->yPosition) - getIntersectionWithY(ed->left, ed->yPosition);
        if (printouts)
            cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
        gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->right, ed);
        // Shift calculation result to get right goal line
        // Shift calculation result to get left goal line
        gld->rightGoalLine = gld->leftGoalLine;
        gld->leftGoalLine.p2.x -= ed->calcRoadSize / 4;
        gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);
        gld->rightGoalLine.p2.x += ed->calcRoadSize / 4;
        gld->rightGoalLine.slope = getLineSlope(gld->rightGoalLine.p2, gld->rightGoalLine.p1);
        gld->confidenceLevel_rightGoalLine = 4;

    }
    else if (foundR) {
        lineUsedForEstimation = ed->right; // used for debug of getRoadSize and getRoadAngle
        whichLine = 0; // used for debug of getRoadSize and getRoadAngle
        if (printouts)
            cout << "Found only right, estimating dash" << endl;
        // Estimate dash:
        calcRoadAngle = getRoadAngle(1, ed->right.slope);
        ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;// * (ed->right.slope/50);
        int expectedDashLineX = getIntersectionWithY(ed->right, ed->yPosition) - ed->calcRoadSize;

        float expectedDashLineAngle = abs(ed->right.slope)
                                      + calcRoadAngle;

        if (printouts) {
            cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
            cout << "calcRoadAngle: " << calcRoadAngle << endl;
            cout << "ed->calcRoadSize: " << ed->calcRoadSize << endl;
            cout << "expectedDashLineX: " << expectedDashLineX << endl;
            cout << "abs(ed->right.slope): " << abs(ed->right.slope) << endl;
        }
        if (expectedDashLineAngle > 90) {
            expectedDashLineAngle = expectedDashLineAngle - 180;
        }
        if (printouts)
            cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
        ed->dash.slope = expectedDashLineAngle;
        ed->dash.p1.x = expectedDashLineX;
        ed->dash.p1.y = ed->yPosition;
        ed->isDashEstimated = true;
        // Calculate right goal line
        gld->rightGoalLine = simple_calculateGoalLine(ed->dash, ed->right, ed);
        gld->confidenceLevel_rightGoalLine = 3;
        // Shift calculation result to get left goal line
        gld->leftGoalLine = gld->rightGoalLine;
        gld->leftGoalLine.p2.x -= ed->calcRoadSize;
        gld->leftGoalLine.slope = getLineSlope(gld->leftGoalLine.p2, gld->leftGoalLine.p1);

    }
    else if (foundL) {
        lineUsedForEstimation = ed->left; // used for debug of getRoadSize and getRoadAngle
        whichLine = 2; // used for debug of getRoadSize and getRoadAngle
        // Estimate dash:
        if (printouts)
            cout << "Found only left, estimating dash" << endl;
        calcRoadAngle = getRoadAngle(3, ed->left.slope);
        ed->calcRoadSize = getRoadSize(calcRoadAngle) * roadSizeAdjustment;
        int expectedDashLineX = getIntersectionWithY(ed->left, ed->yPosition) + ed->calcRoadSize;
        float expectedDashLineAngle = abs(ed->left.slope)
                                      + calcRoadAngle;

        if (printouts) {
            cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
            cout << "calcRoadAngle: " << calcRoadAngle << endl;
            cout << "expectedDashLineX: " << expectedDashLineX << endl;
            cout << "abs(ed->left.slope): " << abs(ed->left.slope) << endl;
        }
        if (expectedDashLineAngle > 90) {
            expectedDashLineAngle = expectedDashLineAngle - 180;
        }
        if (printouts)
            cout << "expectedDashLineAngle: " << expectedDashLineAngle << endl;
        ed->dash.slope = expectedDashLineAngle;
        ed->dash.p1.x = expectedDashLineX;
        ed->dash.p1.y = ed->yPosition;
        ed->isDashEstimated = true;
        // Calculation left goal line
        gld->leftGoalLine = simple_calculateGoalLine(ed->left, ed->dash, ed);
        // Shift calculation result to get right goal line
        gld->rightGoalLine = gld->leftGoalLine;
        gld->rightGoalLine.p2.x += ed->calcRoadSize;
        gld->rightGoalLine.slope = getLineSlope(gld->rightGoalLine.p2, gld->rightGoalLine.p1);
        gld->confidenceLevel_rightGoalLine = 2;
    }

    // Set the global variable used in charasteristicFiltering
    pureRoadSize = getRoadSize(calcRoadAngle);
    calcRoadSize = pureRoadSize;
}

bool isNoneCustomLine(CustomLine aspirant) {
    if (aspirant.p1.x == 0 && aspirant.p1.y == 0 && aspirant.p2.x == 0 && aspirant.p2.y == 0)
        return true;
    return false;
}

int getRoadAngle(int lineDetected, int lineAngle) {
    int roadAngleNow = ROAD_ANGLE; // Previous declaration was roadAngleNow. Shadows the global variable
    float c1 = (roadAngleNow - 29.1)
               / (180 - abs(MID_DASH_ANGLE) - roadAngleNow);
    float c2 = (roadAngleNow - 65.0) / (MID_DASH_ANGLE + 90);
    //cout << "Road angle consts: " << c1 << "," << c2 << endl;
    switch (lineDetected) {
        case 1: {
            //founded line is right line
            if (lineAngle < 63 && lineAngle >= 25) {
                roadAngleNow = 29.1 + c1 * lineAngle; //1.44
            }
            else if (lineAngle < 25) {
                roadAngleNow = 65;
            }
        };
            break;
        case 2: {
            //founded line is dash line
            if (lineAngle < 0) {
                roadAngleNow = 65 + (lineAngle + 90) * c2; //0.59;
            }
            else {
                roadAngleNow = 65 + (lineAngle - 90) * c2; //0.59;
            }
        };
            break;
        case 3: {
            //founded line is left line
            if (lineAngle > -63 && lineAngle <= -25) {
                roadAngleNow = 29.1 - c1 * lineAngle; //1.44
            }
            else if (lineAngle > -25) {
                roadAngleNow = 65;
            }
        };
            break;
    }
    return roadAngleNow;
}

CustomLine simple_calculateGoalLine(CustomLine fst, CustomLine snd, EstimationData *ed) {
    bool printouts = false;
    if (printouts)
        cout << "__start simple_calculateGoalLine" << endl;

    CustomLine goalLine;
    Point vp;
    Point goalP;

    //Set goal height
    goalP.y = ed->yPosition;
    int currRoadSize = ed->calcRoadSize; // STRANGE, why is this used?

    // Get the line equation for fst line
    float da = tan(fst.slope * M_PI / 180);
    float db = fst.p1.y - fst.p1.x * da;
    int fstGoalX = (goalP.y - db) / da; // Local var needed, the line may be estimated

    // Get the line equation for the snd line
    float a = tan(snd.slope * M_PI / 180);
    float b = snd.p1.y - snd.p1.x * a;
    int sndGoalX = (goalP.y - b) / a; // Local var needed, the line may be estimated

    //Calculate vanishing point
    //if (da != a) { To avoid float-equal warning
    if (fabs(da - a) > 0.001) {
        vp.x = (b - db) / (da - a);
    }
    else {
        // Use some default value???
    }
    vp.y = da * vp.x + db;


    int roadSz = (sndGoalX - fstGoalX);
    if (ed->isDashEstimated || ed->isRightEstimated || ed->isLeftEstimated) {
        goalP.x = fstGoalX + ed->calcRoadSize * ROAD_GOAL;
    }
    else {
        goalP.x = fstGoalX + roadSz * ROAD_GOAL;//(fstGoalX + otherGoalX)/2;//fstGoalX + ROAD_SIZE/2;
    }

    if (printouts) {
        cout << "vp (" << vp.x << "," << vp.y << ")" << endl;
        cout << "goalP (" << goalP.x << "," << goalP.y << ")" << endl;
    }
    // ----- Debug stuff follows

    int fstCenterX = (fst.p1.x + fst.p2.x) / 2; // Only for debug
    int fstCenterY = (fst.p1.y + fst.p2.y) / 2; // Only for debug

    //mylog << ltu.fstLine.slope << "," << fstCenterX << "," << fstCenterY << "," << roadSz << "," << (180 - abs(ltu.fstLine.slope) - abs(ltu.rightLine.slope)) << endl;
    if (printouts) {
        cout << "Road size: " << roadSz << endl;
        cout << "Road angle prediction: " << calcRoadAngle << endl;
        cout << "Road size prediction: " << ed->calcRoadSize << endl;
        cout << "Road angle: "
        << (180 - abs(fst.slope)
            - abs(ed->right.slope)) << endl;
        cout << "snd line X: " << sndGoalX << endl;
    }
    //-----------------------
    // Set up goal line
    //-----------------------

    //If we have a goal set the position
    if (printouts) {
        cout << "Road size diff: " << abs(currRoadSize - ed->calcRoadSize) << endl;
    }
    if (abs(currRoadSize - ed->calcRoadSize) < 0.5 * currRoadSize) // Strange, this will always be true
    {
        //Set your goal
        goalLine.p1 = vp;
        goalLine.p2 = goalP;
        goalLine.slope = getLineSlope(vp, goalP);
    }
    else {
        cout << "Road size diff to high, no goalLine will be provided " << endl;
        goalLine = getNoneCustomLine();
    }
    if (printouts)
        cout << "__end simple_calculateGoalLine" << endl;

    return goalLine;
}

int getIntersectionWithY(CustomLine l, int y) {
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if (abs(a) > 0.001) {
        positionX = (y - b) / a;
    }
    return positionX;
}

int getRoadSize(int roadAngleVal) {
    bool printouts = false;
    int roadSizeNow = ROAD_SIZE; // Previous declaration was roadSizeNow. Shadows the global variable

    if (roadAngleVal > ROAD_ANGLE && roadAngleVal < (ROAD_ANGLE + 15)) {
        roadSizeNow = 5 * roadAngleVal + (ROAD_SIZE - ROAD_ANGLE * 5);
    }
    else if (roadAngleVal > (ROAD_ANGLE + 15)) {
        float a = (ROAD_SIZE - 5) / 5;
        float b = 3 * ROAD_SIZE - (ROAD_ANGLE + 25) * a;
        roadSizeNow = roadAngleVal * a + b;
    }
    else if (roadAngleVal > (ROAD_ANGLE - 15) && roadAngleVal < ROAD_ANGLE) {
        roadSizeNow = 5 * (2 * ROAD_ANGLE - roadAngleVal)
                      + (ROAD_SIZE - ROAD_ANGLE * 5);
    }
    else if (roadAngleVal < (ROAD_ANGLE - 15)
             && roadAngleVal > (ROAD_ANGLE - 25)) {
        //cout << "SZ S" << endl;
        float a = (ROAD_SIZE - 5) / 5;
        float b = 3 * ROAD_SIZE - (ROAD_ANGLE + 25) * a;
        roadSizeNow = (2 * ROAD_ANGLE - roadAngleVal) * a + b;
    }

    return roadSizeNow;
}

int getIntersectionWithTop(CustomLine l) {
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if (abs(a) > 0.001) {
        positionX = (0 - b) / a;
    }
    return positionX;
}

int getIntersectionWithTopP2(CustomLine l)
{
    float a = tan(M_PI * l.slope / 180);
    float b = l.p2.y - l.p2.x * a;
    int positionX = l.p2.x;
    if (abs(a) > 0.001)
    {
        positionX = (0 - b) / a;
    }
    return positionX;
}

PolySize createPolySize(const RotatedRect &rect)
{
    Point2f rect_points[4];
    rect.points (rect_points);
    int sizeX = 0, sizeY = 0, sizeR = 0;
    Point shortSideMiddle;
    Point longSideMiddle;
    // Find rect sizes
    for (int j = 0; j < 4; j++)
    {
        //cout << "Size:" << sizeR << endl;
        if (sizeX == 0)
        {
            sizeX = sizeR;
            shortSideMiddle.x = (rect_points[j].x + rect_points[(j + 1) % 4].x)
                                / 2;
            shortSideMiddle.y = (rect_points[j].y + rect_points[(j + 1) % 4].y)
                                / 2;
        }
        else if (sizeY == 0 && sizeR != sizeX)
        {
            sizeY = sizeR;
            longSideMiddle.x = (rect_points[j].x + rect_points[(j + 1) % 4].x)
                               / 2;
            longSideMiddle.y = (rect_points[j].y + rect_points[(j + 1) % 4].y)
                               / 2;
        }
    }
    if (sizeX > sizeY)
    {
        Point2f temp;
        sizeR = sizeX;
        sizeX = sizeY;
        sizeY = sizeR;
        temp = longSideMiddle;
        longSideMiddle = shortSideMiddle;
        shortSideMiddle = temp;
    }

    PolySize polysize =
            { sizeX, sizeY, sizeR, shortSideMiddle, longSideMiddle };
    return polysize;

}
/***
 * STOP ALGORITHM HELPER FUNCTIONS
 */

/***
 * START SHOWCASE FUNCTIONS
 */
void displayContours() {
    Mat out = Mat(image.size().height, image.size().width, CV_32F);

    for (int i = 0; i < contours.size(); i++) {
        Scalar color = Scalar(255, 255, 255);
        cv::drawContours(out, contours, i, color, 1, 8, hierarchy, 0, Point());
    }

    imshow("Contours", out);
}

void displayPolygonContours() {
    Mat out = Mat(image.size().height, image.size().width, CV_32F);

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        Scalar color = Scalar(255, 255, 255);
        drawContours(out, contours_poly, i, color, 1, 8, hierarchy, 0, Point());
    }

    imshow("Polygon Contours", out);
}

void displayBoundingBoxes() {
    Mat out;
    originalImage.copyTo(out);

    RotatedRect rect;

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        rect = minAreaRect(contours_poly[i]);
        Point2f rect_points[4];
        rect.points(rect_points);

        for (int j = 0; j < 4; j++) {
            line(out, rect_points[j], rect_points[(j + 1) % 4], Scalar(0, 0, 255), 2);
        }
    }

    imshow("Bounding Boxes", out);
}

void displayDashedLines() {
    Mat out;
    originalImage.copyTo(out);

    Scalar red = Scalar(0, 0, 255);
    for (int i = 0; i < dashLines.size(); i++) {
        line(out, dashLines[i].p1, dashLines[i].p2, red, 3, 8, 0);
    }

    imshow("Dashed Lines", out);
}

void displaySolidLines() {
    Mat out;
    originalImage.copyTo(out);

    Scalar red = Scalar(0, 0, 255);
    for (int i = 0; i < solidLines.size(); i++) {
        line(out, solidLines[i].p1, solidLines[i].p2, red, 2, 8, 0);
    }

    imshow("Solid Lines", out);
}

void displaySelectedLines() {
    Mat out;
    originalImage.copyTo(out);

    Scalar blue = Scalar(255, 0, 0);
    Scalar green = Scalar(0, 255, 0);
    Scalar red = Scalar(0, 0, 255);
    Scalar orange = Scalar(0, 165, 255);

    if (ltu.foundD) {
        line(out, ltu.dashLine.p1, ltu.dashLine.p2, red, 2, 8, 0);
    }

    if (ltu.foundL) {
        line(out, ltu.leftLine.p1, ltu.leftLine.p2, blue, 2, 8, 0);
    }

    if (ltu.foundR) {
        line(out, ltu.rightLine.p1, ltu.rightLine.p2, green, 2, 8, 0);
    }

    if (ltu.dashedCurveFound) {
        line(out, ltu.dashedCurve[1].p1, ltu.dashedCurve[1].p2, orange, 2, 8, 0);
    }

    imshow("Selected Lines", out);
};

void displayTrajectory() {
    Mat out;
    originalImage.copyTo(out);

    auto data = dataToDriver[0];
    auto leftGoalLine = data.leftGoalLines0;
    auto rightGoalLine = data.rightGoalLines0;
    auto currentLine = data.currentLine;

    Point2f intersectionPoint;
    getVanishingPoint(leftGoalLine.p1, leftGoalLine.p2, rightGoalLine.p1, rightGoalLine.p2, intersectionPoint);

    Scalar blue = Scalar(255, 0, 0);
    Scalar green = Scalar(0, 255, 0);
    Scalar orange = Scalar(0, 165, 255);
    Scalar red = Scalar(0, 0, 255);
    line(out, leftGoalLine.p1, leftGoalLine.p2, blue, 2, 8, 0);
    line(out, rightGoalLine.p1, rightGoalLine.p2, green, 2, 8, 0);
    line(out, currentLine.p1, currentLine.p2, orange, 2, 8, 0);
    line(out, Point{(int) intersectionPoint.x, (int) intersectionPoint.y},
         Point{(int) intersectionPoint.x, h/2}, red, 2, 8, 0);

    imshow("Trajectory", out);
}

void displayBothLines(string title) {
    Mat out;
    originalImage.copyTo(out);

    Scalar red = Scalar(0, 0, 255);
    for (int i = 0; i < dashLines.size(); i++) {
        line(out, dashLines[i].p1, dashLines[i].p2, red, 3, 8, 0);
    }

    Scalar orange = Scalar(0, 165, 255);
    for (int i = 0; i < solidLines.size(); i++) {
        line(out, solidLines[i].p1, solidLines[i].p2, orange, 2, 8, 0);
    }

    imshow(title, out);
}

bool getVanishingPoint(Point2f o1, Point2f p1, Point2f o2, Point2f p2, Point2f &r)
{
    Point2f x = o2 - o1;
    Point2f d1 = p1 - o1;
    Point2f d2 = p2 - o2;

    float cross = d1.x*d2.y - d1.y*d2.x;
    if (abs(cross) < /*EPS*/1e-8)
        return false;

    double t1 = (x.x * d2.y - x.y * d2.x)/cross;
    r = o1 + d1 * t1;
    return true;
}
/***
 * STOP SHOWCASE FUNCTIONS
 */

#include "main.h"

int main(int argc, char **argv) {
    m_config.XTimesYMin = 2;
    m_config.XTimesYMax  = 20;
    m_config.maxY = 235;
    m_config.maxArea = 4;

    char *imageName = argv[1];
    if (!readImage(imageName, argc)) {
        printf(" No image data \n ");
        return -1;
    };
    //imshow("Original Image", image);

    toGrayScale();
    //imshow("GrayScale Image", image);

    cropImage();
    //imshow("Croped Image", image);

    image.copyTo(originalImage);
    cvtColor(originalImage, originalImage, CV_GRAY2BGR);

    applyAndDisplayThreshold();

    getAndDisplayContours();

    getAndDisplayPolygonContours();

    getAndDisplayRectangles();

    classifyLines();



    filterAndMerge();

    finalFilter();

    displayDashedLines();
    displaySolidLines();

    characteristicFiltering(&ltu);

    displaySelectedLines();

    waitKey(0);

    return 0;
}

int readImage(char *imageName, int argc) {
    image = imread(imageName, 1);
    if (argc != 2 || !image.data) return 0;
    return 1;
}

void toGrayScale() {
    // one channel
    // binary image
    cvtColor(image, image, CV_BGR2GRAY);
}

void cropImage() {
    int height = image.size().height;
    int width = image.size().width;
    image = image(cv::Rect(1, 2 * height / 16 - 1, width - 1, 10 * height / 16 - 1));
}

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

void applyAndDisplayThreshold() {
    threshold(image, image, getDynamicThresh(-2), 255, CV_THRESH_BINARY);
    imshow("Threshold Image Lux -2", image);
}

void getAndDisplayContours() {
    vector<Vec4i> hierarchy;
    cntDash = 0;
    cntSolid = 0;

    findContours(image, contours, hierarchy, CV_RETR_TREE,
                 CV_CHAIN_APPROX_SIMPLE, Point(0, 0));
    //Mat drawing
    Mat out = Mat(image.size().height, image.size().width, CV_32F);
    for (int i = 0; i < contours.size(); i++) {
        Scalar color = Scalar(255, 255, 255);
        cv::drawContours(out, contours, i, color, 1, 8, hierarchy, 0, Point());
    }
    imshow("Contours", out);
}

void getAndDisplayPolygonContours() {
    contours_poly.resize(contours.size());
    dashLines = vector<CustomLine>(contours.size());
    solidLines = vector<CustomLine>(contours.size());
    for (unsigned int i = 0; i < contours.size(); i++) {
        approxPolyDP(Mat(contours[i]), contours_poly[i], 3, true);
    }

    Mat out = Mat(image.size().height, image.size().width, CV_32F);
    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        Scalar color = Scalar(255, 255, 255);
        drawContours(out, contours_poly, i, color, 1, 8, vector<Vec4i>(), 0, Point());
    }
    imshow("Polygon Contours", out);
}

void getAndDisplayRectangles() {
    RotatedRect rect;

//    Mat out = Mat(image.size().height, image.size().width, CV_32F);
    Mat out;
    originalImage.copyTo(out);

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        rect = minAreaRect(contours_poly[i]);
        Point2f rect_points[4];
        rect.points(rect_points);
//            rects.push_back(rect);
        int sizeX = 0, sizeY = 0, sizeR = 0;
        Point shortSideMiddle;
        Point longSideMiddle;
        // Find rect sizes
        for (int j = 0; j < 4; j++) {
            sizeR = cv::sqrt(
                    cv::pow((rect_points[j].x - rect_points[(j + 1) % 4].x), 2)
                    + cv::pow(
                            (rect_points[j].y
                             - rect_points[(j + 1) % 4].y), 2));
            if (sizeX == 0) {
                sizeX = sizeR;
                shortSideMiddle.x = (rect_points[j].x
                                     + rect_points[(j + 1) % 4].x) / 2;
                shortSideMiddle.y = (rect_points[j].y
                                     + rect_points[(j + 1) % 4].y) / 2;
            }
            else if (sizeY == 0 && sizeR != sizeX) {
                sizeY = sizeR;
                longSideMiddle.x = (rect_points[j].x
                                    + rect_points[(j + 1) % 4].x) / 2;
                longSideMiddle.y = (rect_points[j].y
                                    + rect_points[(j + 1) % 4].y) / 2;
            }

            line(out, rect_points[j], rect_points[(j + 1) % 4], Scalar(0, 0, 255), 2);
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

    imshow("Bounding Boxes", out);

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

void displayDashedLines() {
//    Mat out = Mat(image.size().height, image.size().width, CV_32F);
    Mat out;
    originalImage.copyTo(out);

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        Scalar color = Scalar(0, 0, 255);
        //draw lines
        for (int i = 0; i < dashLines.size(); i++) {
            line(out, dashLines[i].p1, dashLines[i].p2, color, 3, 8, 0);

        }
    }

    imshow("Dashed Lines", out);
}

void classifyLines() {
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
        } else if (area > m_config.maxArea * 10000)
        {
            minXI = w;
            minYI = h;
            for (int j = 0; j < 4; j++)
            {
                if (minXI > rect_points[j].x)
                {
                    minXI = rect_points[j].x;
                }
                if (minYI > rect_points[j].y)
                {
                    minYI = rect_points[j].y;
                }
            }
            YI = rectCenter.y;
            intersectionRect = i;
            bigRect = rects[i];
            cout << "area: " << area << endl;
            cout << "intersectionRect: " << intersectionRect << endl;

            //intersectionOn = true;
            //foundIntersection = true;
            float angle_thr = 10;
            float height_thr = (1*h)/2;
            if ( (abs(rect.angle) < angle_thr || 180 - abs(rect.angle) < angle_thr ) && rectCenter.y > height_thr && roadState == NORMAL)
            {
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
        if (time_taken_contour > 800){
            cout << "roadState set to NORMAL do TIMEOUT" << endl;
            roadState = NORMAL;
            intersectionOn = false;
            foundIntersection = false;
            calcIntersectionGoalLine = false;
        }

        if (intersectionRect == -1 && roadState == INTERSECTION){
            cout << "STOPS updating intersection_goalLine: " << endl;
            calcIntersectionGoalLine = false;
        }

        if (intersectionOn && !foundIntersection)
        {
            YI = h;


    }
}

void displaySolidLines() {
    Mat out;
    originalImage.copyTo(out);
    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        Scalar color = Scalar(0, 0, 255);
        for (int i = 0; i < solidLines.size(); i++) {
            line(out, solidLines[i].p1, solidLines[i].p2, color, 2, 8, 0);
        }
    }
    imshow("Solid Lines", out);
}

void filterAndMerge() {
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

void finalFilter()
{
    for (int i = 0; i < cntSolid; i++)
    {
        CustomLine l = solidLines[i];
        int minAngle = MIN_ANGLE - 5;
        if (abs(l.slope) < minAngle)
        {
            solidLines[i] = solidLines[cntSolid - 1];
            cntSolid--;
            if (i > 0)
            {
                i--;
            }
            foundStopStartLine = true;
        }
    }

    //Dash also positioned too high on the image or too left or too right
    int maxDashY = 0;
    for (int i = 0; i < cntDash; i++)
    {
        CustomLine l = dashLines[i];
        int dashCenterX = (l.p1.x + l.p2.x) / 2;
        int dashCenterY = (l.p1.y + l.p2.y) / 2;
        if ((l.slope < MIN_ANGLE) && (l.slope > ((-1) * MIN_ANGLE))
            || (dashCenterY < h / 15) || (dashCenterX > 19 * w / 20)) //|| (dashCenterX < w/20) too left //too high
        {
            dashLines[i] = dashLines[cntDash - 1];
            cntDash--;
            if (i > 0)
            {
                i--;
            }
        }

        if (maxDashY < max(dashLines[i].p1.y, dashLines[i].p2.y))
        {
            maxDashY = max(dashLines[i].p1.y, dashLines[i].p2.y);
        }
    }

}

void characteristicFiltering(LinesToUse *ltu)
{
    if (roadState == INTERSECTION){
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
    if (cntDash > 0)
    {
        ltu->dashLine.p1.y = 0;
        ltu->dashLine.p2.y = 0;
        // Sort the dashes by highest p1.y value first
        std::sort(dashLines.begin(), dashLines.begin() + cntDash);
        ltu->cntDash = cntDash;
        // Make p1 be the bottom point
        for (int i = 0; i < cntDash; i++)
        {
            if (dashLines[i].p1.y < dashLines[i].p2.y)
            {
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
        if (cntDash > 1)
        {
            // Copy needed data
            std::vector<CustomLine> dashedLines = dashLines;
            std::vector<CustomLine> unusedLines;
            int cntDashed = cntDash;

            // The found curves will be stored in this
            std::vector<vector<CustomLine> > curves;

            for (int i = 0; i < cntDash - 1; i++)
            {
                // When fiding curve from i, dashes present at i < don't need to be checked.
                std::vector<CustomLine> lines;
                for (int j = 0; j < cntDashed; j++)
                {
                    lines.push_back(dashedLines[j]);
                }

                std::vector<CustomLine> res = findCurve(lines);
                if (res.size() == 0)
                {
                    unusedLines.push_back(dashedLines[0]);
                    dashedLines.erase(dashedLines.begin());
                    cntDashed--;
                    continue;
                }
                // If curve found, we proceed
                std::vector<CustomLine> curve;
                curve.push_back(lines[0]);

                for (int j = 0; j < res.size(); j++)
                {
                    curve.push_back(res[j]);
                }
                // Save the found curve
                curves.push_back(curve);

                // Remove dashed lines already a part of a found curve
                for (int j = 0; j < curve.size(); j++)
                {
                    for (int k = 0; k < cntDashed; k++)
                    {
                        if (curve[j] == dashedLines[k])
                        {
                            dashedLines.erase(dashedLines.begin() + k);
                            cntDashed--;
                        }
                    }
                }
                // Check if we got enough lines to make another curve
                if (cntDashed < 2)
                {
                    break;
                }
            }
            // Pick which curve to use
            int maxY = 0;
            for (int i = 0; i < curves.size(); i++)
            {
                int dashSupPosX = getIntersectionWithBottom(curves[i][0]);

                if (curves[i][0].p1.y > maxY &&
                    (abs(dashSupPosX - currentDashGoalX) < calcRoadSize * 0.8
                     || currentDashGoalX == 0)){
                    dashLines[0] = curves[i][0];
                    ltu->dashedCurve = curves[i];
                    ltu->dashedCurveFound = true;
                    maxY = curves[i][0].p1.y;

                }
            }
//            global_dashedCurve = ltu->dashedCurve;
            // Check if any remaining dashed lines not a part of a curve could be potential
            // left or right lines.
            if (ltu->dashedCurveFound)
            {
                for (int j = 0; j < cntDashed; j++)
                {
                    unusedLines.push_back(dashedLines[j]);
                }

                for (int i = 0; i < unusedLines.size(); i++)
                {
                    int s = ltu->dashedCurve.size() - 1;

                    // Checks if a unused dash line is interleaved with the found dashed curve w.r.t the y-axis
                    if (!(((unusedLines[i].p1.y > ltu->dashedCurve[0].p1.y) &&
                           (unusedLines[i].p2.y > ltu->dashedCurve[0].p1.y)) ||
                          ((unusedLines[i].p1.y < ltu->dashedCurve[s].p2.y) &&
                           (unusedLines[i].p2.y < ltu->dashedCurve[s].p2.y))))
                    {
                        // Filter away short lines that probably do not have an accurate angle
                        if (getDist(unusedLines[i].p1, unusedLines[i].p2) > (m_config.maxY / 3)){
                            solidLines[cntSolid] = unusedLines[i];
                            cntSolid++;
                        }else{
                        }
                    }
                }
                // set currentDashGoalX
                ltu->dashLine = ltu->dashedCurve[0];
                currentDashGoalX = getIntersectionWithBottom(ltu->dashLine);
                ltu->dashLineVec = Vec4i(ltu->dashLine.p1.x, ltu->dashLine.p1.y, ltu->dashLine.p2.x, ltu->dashLine.p2.y);
                ltu->foundD = true;
            }
        }

        //
        // The old way to do it follows
        //
        if (!ltu->dashedCurveFound)
        {
            for (int i = 0; i < cntDash; i++)
            {
                for (int j = 0; j < cntDash; j++)
                {
                }
                if (i != cntDash - 1 && max(dashLines[i + 1].p1.y, dashLines[i + 1].p2.y)
                                        > min(dashLines[i].p1.y, dashLines[i].p2.y))
                {
                    int positionX = getIntersectionWithBottom(dashLines[i]);
                    int nPositionX = getIntersectionWithBottom(dashLines[i + 1]);
                    if (abs(currentDashGoalX - positionX)
                        < abs(currentDashGoalX - nPositionX))
                    {
                        dashLines.erase(dashLines.begin() + 1);
                        cntDash--;
                    }
                    else
                    {
                        dashLines.erase(dashLines.begin());
                        cntDash--;
                    }
                    if (i > 0)
                    {
                        i--;
                    }
                }

                if (intersectionOn && ((dashLines[i].p1.y + dashLines[i].p2.y) / 2) < YI)
                {
                    dashLines[i] = dashLines[cntDash - 1];
                    cntDash--;
                    if (i > 0)
                    {
                        i--;
                    }
                }
            }
            if (cntDash > 0)
            {
                ltu->dashLine = dashLines[0];
                int dashSupPosX = getIntersectionWithBottom(ltu->dashLine);
                //if(ltu->dashLine.slope < 0) {
                if (abs(dashSupPosX - currentDashGoalX) < calcRoadSize * 0.8
                    || currentDashGoalX == 0)
                {
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
    if (cntSolid > 0 && !intersectionOn)
    {
        ltu->rightLine.p1.x = w + 50;
        ltu->rightLine.p2.x = w + 50;
        for (int i = 0; i < cntSolid; i++)
        {
            if (solidLines[i].slope < 90 && solidLines[i].slope > 0
                && min(solidLines[i].p1.x, solidLines[i].p2.x)
                   < min(ltu->rightLine.p1.x, ltu->rightLine.p2.x))
            {
                ltu->rightLine = solidLines[i];
                ltu->foundR = true;
            }
        }
        if (ltu->foundR)
        {
            int rSupPosX = getIntersectionWithBottom(ltu->rightLine);

            if (abs(rSupPosX - currentRightGoalX) < calcRoadSize * 0.8
                || currentRightGoalX == 0)
            {
                ltu->rightLineVec = Vec4i(ltu->rightLine.p1.x, ltu->rightLine.p1.y,
                                          ltu->rightLine.p2.x, ltu->rightLine.p2.y);
                currentRightGoalX = rSupPosX;
            }
            else
            {
                ltu->foundR = false;
            }
        }

        ltu->leftLine.p1.x = -50;
        ltu->leftLine.p2.x = -50;
        for (int i = 0; i < cntSolid; i++)
        {
            // centerSolidLineX commented to avoid unused variable warning!
            //int centerSolidLineX = (solidLines[i].p1.x + solidLines[i].p2.x)/2;
            if (solidLines[i].slope > -90 && solidLines[i].slope < 0
                && min(solidLines[i].p1.x, solidLines[i].p2.x)
                   > min(ltu->leftLine.p1.x, ltu->leftLine.p2.x))
            {
                ltu->leftLine = solidLines[i];
                ltu->foundL = true;
            }
        }
        if (ltu->foundL)
        {
            int lSupPosX = getIntersectionWithBottom(ltu->leftLine);
            if (abs(lSupPosX - currentLeftGoalX) < calcRoadSize * 0.8
                || currentLeftGoalX == 0)
            {
                ltu->leftLineVec = Vec4i(ltu->leftLine.p1.x, ltu->leftLine.p1.y,
                                         ltu->leftLine.p2.x, ltu->leftLine.p2.y);
                currentLeftGoalX = lSupPosX;
            }
            else
            {
                ltu->foundL = false;
            }
        }
    }

    //////////////////
    // This check is used to find out if we have passed an intersection.
    // If it is passed, it will set the roadState to NORMAL.
    //////////////////
    if(roadState == INTERSECTION){
        int minSlope = 40;

        int maxY = 0;

        if (ltu->dashedCurve.size() > 1){
            for (int i = 0; i < ltu->dashedCurve.size(); i ++){
                if (maxY < ltu->dashedCurve[i].p1.y){ // p1 is always the point furtest down on the frame
                    maxY = ltu->dashedCurve[i].p1.y;
                }
            }

            if (abs(ltu->dashedCurve[0].slope) > minSlope && maxY > (8 * h / 10)){

                roadState = NORMAL;
                intersectionOn = false;
                foundIntersection = false;
                calcIntersectionGoalLine = false;
            }
        }else if(ltu->foundR){
            maxY = ltu->rightLine.p1.y;
            if (abs(ltu->rightLine.slope) > minSlope && maxY > (8 * h / 10)){
                roadState = NORMAL;
                intersectionOn = false;
                foundIntersection = false;
                calcIntersectionGoalLine = false;

            }
        }else if(ltu->foundL){
            maxY = ltu->leftLine.p1.y;
            if (abs(ltu->leftLine.slope) > minSlope && maxY > (8 * h / 10)){
                roadState = NORMAL;
                intersectionOn = false;
                foundIntersection = false;
                calcIntersectionGoalLine = false;

            }
        }
    }
    return;
}

int getIntersectionWithBottom(CustomLine l)
{
    float a = tan(M_PI * l.slope / 180);
    float b = l.p1.y - l.p1.x * a;
    int positionX = l.p1.x;
    if (abs(a) > 0.001)
    {
        positionX = (h - b) / a;
    }
    return positionX;
}

vector<CustomLine> findCurve(vector<CustomLine> lines)
{
    // used to be sure that you have found the dashes.
    std::vector<CustomLine> curve;
    if (lines.size() < 2)
    {
        return curve;
    }
    for (int j = 1; j < lines.size(); j++)
    {
        // The snd dash has to be above the fst one
        if (lines[0].p2.y > lines[j].p1.y)
        {
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

                if (lines.size() > 2)
                {
                    // Call recursively, but take away the already processed line
                    lines[0] = lines[j];
                    lines.erase(lines.begin() + j);
                    std::vector<CustomLine> res = findCurve(lines);

                    // Add the recursive result
                    for (int j = 0; j < res.size(); j++)
                    {
                        curve.push_back(res[j]);
                    }
                    return curve;
                }
                else
                {
                    return curve;
                }
            }
        }
    }
    return curve;
}

float getDist(const Point p1, const Point p2)
{
    return sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2));
}

void displaySelectedLines() {
    Mat out;
    originalImage.copyTo(out);

    Scalar blue = Scalar(255, 0, 0);
    Scalar green = Scalar(0, 255, 0);
    Scalar red = Scalar(0, 0, 255);

    if (ltu.foundD) {
        line(out, ltu.dashLine.p1, ltu.dashLine.p2, red, 2, 8, 0);
    }

    if (ltu.foundL) {
        line(out, ltu.leftLine.p1, ltu.leftLine.p2, blue, 2, 8, 0);
    }

    if (ltu.foundR) {
        line(out, ltu.rightLine.p1, ltu.rightLine.p2, green, 2, 8, 0);
    }



    imshow("Selected Lines", out);
};
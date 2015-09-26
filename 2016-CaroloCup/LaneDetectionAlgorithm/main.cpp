#include <iostream>
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/highgui/highgui.hpp"
/// example to show how to use measure_time.h
//#include "measure_time.h"

using namespace std;
using namespace cv;

class CustomLine {
public:
    CustomLine() :
            p1(),
            p2(),
            slope(0),
            polygonIndex(-1) // This is a mapping to the polygon that the CustomLine was derived from.
    { }

    virtual ~CustomLine() { }

    bool operator<(const CustomLine &other) const {
        return max(p1.y, p2.y) > max(other.p1.y, other.p2.y);
        //return slope < other.slope;
    }

    bool operator==(const CustomLine &other) const {
        if ((p1.y == other.p1.y) && (p1.x == other.p1.x)) {
            return true;
        }
        return false;
    }

    Point p1, p2;
    float slope;
    int polygonIndex;
};

struct PolySize {
    int sizeX, sizeY, sizeR;
    Point shortSideMiddle;
    Point longSideMiddle;
};

Mat image;
int previousThresh = 48;
vector<vector<Point> > contours_poly;
vector<vector<Point> > contours;
vector<CustomLine> dashLines;
vector<PolySize> line_sizes;
vector<RotatedRect> rects;

int cntDash = 0;


int readImage(char *imageName, int argc);

void toGrayScale();

void cropImage();

int getDynamicThresh(int lux);

void applyThreshold();

Mat getContours();

Mat getPolygonContours();

Mat getRectangles();

void classificationDashedLines();

float getLineSlope(Point &p1, Point &p2);

CustomLine createLineFromRect(RotatedRect *rect, int sizeX, int sizeY, int polygonIndex);

Mat getDashedLines();

int main(int argc, char **argv) {
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

    applyThreshold();
    //imshow("Threshold Image Lux 50", image);

    Mat imageContour = getContours();
    //imshow("Contours", imageContour);

    Mat imagePolygonCountour = getPolygonContours();
    //imshow("Polygon Contours", imagePolygonCountour);

    Mat imageBoundingBox = getRectangles();
    //imshow("Bounding Box", imageBoundingBox);

    classificationDashedLines();

    Mat imageDashedLines = getDashedLines();
    imshow("Classified Dashed Lines", imageDashedLines);


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

void applyThreshold() {
    threshold(image, image, getDynamicThresh(50), 255, CV_THRESH_BINARY);
}

Mat getContours() {
    vector<Vec4i> hierarchy;
    cntDash = 0;

    findContours(image, contours, hierarchy, CV_RETR_TREE,
                 CV_CHAIN_APPROX_SIMPLE, Point(0, 0));
    //Mat drawing
    Mat out = Mat::zeros(image.size().height, image.size().width, CV_32F);
    for (int i = 0; i < contours.size(); i++) {
        Scalar color = Scalar(255, 255, 255);
        cv::drawContours(out, contours, i, color, 2, 8, hierarchy, 0, Point());
    }
    return out;
}

Mat getPolygonContours() {
    contours_poly.resize(contours.size());
    dashLines = vector<CustomLine>(contours.size());

    for (unsigned int i = 0; i < contours.size(); i++) {
        approxPolyDP(Mat(contours[i]), contours_poly[i], 3, true);
    }

    Mat out = Mat::zeros(image.size().height, image.size().width, CV_32F);
    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        Scalar color = Scalar(255, 255, 255);
        drawContours(out, contours_poly, i, color, 1, 8, vector<Vec4i>(), 0, Point());
    }
    return out;
}

Mat getRectangles() {
    bool picture = false;
    RotatedRect rect;

    Mat out = Mat::zeros(image.size().height, image.size().width, CV_32F);

    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        rect = minAreaRect(contours_poly[i]);
        Point2f rect_points[4];
        rect.points(rect_points);
//            rects.push_back(rect);
        //cout << "Angle: " << rect.angle << endl;
        int sizeX = 0, sizeY = 0, sizeR = 0;
        Point shortSideMiddle;
        Point longSideMiddle;
        // Find rect sizes
        for (int j = 0; j < 4; j++) {
            //cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y << "]" << endl;
            sizeR = cv::sqrt(
                    cv::pow((rect_points[j].x - rect_points[(j + 1) % 4].x), 2)
                    + cv::pow(
                            (rect_points[j].y
                             - rect_points[(j + 1) % 4].y), 2));
            //cout << "Size:" << sizeR << endl;
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

            line(out, rect_points[j], rect_points[(j + 1) % 4], Scalar(255, 0, 0));
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

    return out;

}

void classificationDashedLines() {
    bool printouts = true;
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

        if (sizeY > 2 * sizeX
            && sizeY < 20 * sizeX
            && sizeY < 235) {
            dashLines[cntDash] = createLineFromRect(&rect, sizeX, sizeY, i);
            cntDash++;
            cout << "Dash Rect y: " << rectCenter.y << endl;
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
    bool printouts = false;
    if (printouts)
        cout << "__start createLineFromRect" << endl;

    Point2f rect_points[4];
    rect->points(rect_points);

    if (printouts) {
        cout << "Sizes: " << sizeX << " " << sizeY << endl;;
        for (int j = 0; j < 4; j++)
            cout << "Point [x,y] = [" << rect_points[j].x << "," << rect_points[j].y << "]" << endl;
    }

    CustomLine l;
    Point pt1, pt2;
    l.polygonIndex = polygonIndex;
    //cout << "[centerx, centery] = [" << rect->center.x << "," << rect->center.y << "]" << endl;
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
    //cout << "Angle: " << rect->angle << endl;
    //cout << "[x, y] = [" << pt1.x << "," << pt1.y << "]" << endl;
    l.p1 = pt1;
    l.p2 = pt2;
    l.slope = rect->angle;
    if (printouts)
        cout << "__end createLineFromRect" << endl;
    return l;
}

Mat getDashedLines() {
    Mat out = Mat::zeros(image.size().height, image.size().width, CV_32F);
    for (unsigned int i = 0; i < contours_poly.size(); i++) {
        Scalar color = Scalar(255, 0, 0);
        //draw lines
        for (int i = 0; i < dashLines.size(); i++) {
            line(out, dashLines[i].p1, dashLines[i].p2, color, 1, 8, 0);

        }
    }
    return out;
}














#include "carcalculations.h"

using namespace std;

double angle = 90;
int call_sequence = 0;
CvFont font1;


CarCalculations::CarCalculations(){
}

void CarCalculations::initFont() {
        double hscale1 = 0.4;
        double vscale1 = 0.3;
        double shear1 = 0.2;
        int thickness1 = 1;
        int line_type1 = 6;

        cvInitFont(&font1, CV_FONT_HERSHEY_DUPLEX, hscale1, vscale1, shear1,
                        thickness1, line_type1);

}

void CarCalculations::drawline(IplImage* color_dst, int maxXR, int maxXL, int maxYR, int maxYL,
                CvScalar colorForLines) {
        initFont();
        CvScalar red = CV_RGB(255, 0, 0);

        char xy[255];
        sprintf(xy, "(% d, %d)", (int) maxXL, (int) maxYL);

        char xy1[255];
        sprintf(xy1, "(% d, %d)", (int) maxXR, (int) maxYR);
        char dst1[255];
        sprintf(dst1, "%d", (int) 320 - maxXL);
        char dst2[255];
        sprintf(dst2, "%d", (int) maxXR - 320);

        const char* text = "o";
        char dst3[255];
        sprintf(dst3, "%d, %d", (int) (maxXR + maxXL) / 2, (int) maxYR);

        //dot
        int m = (int) (maxXR + maxXL) / 2;
        cvPutText(color_dst, text, cvPoint(m, maxYR), &font1, red);

        cvLine(color_dst, cvPoint(maxXL, maxYL), cvPoint(maxXR, maxYR),
                        colorForLines, 1, 8);//CV_RGB(128, 255, 255)


        maxYR = 0;
        maxXR = 0;

}

double CarCalculations::calcSumDegree(int count, CvPoint* middle) {

        double theta = 0;
        double slope = 0;
        double dx = 0;
        double dy = 0;
        double degree = 0;
        double sumDegree = 0;

        for (int j = 0; j < count - 1; j++) {
                dx = (middle[j].x - middle[j + 1].x);// x coordinate difference
                dy = (middle[j].y - middle[j + 1].y);// y coordinate difference

                // quadrant I
                if (dx > 0 && dy >= 0) {
                        slope = dy / dx;
                        theta = atan(slope);
                        degree = (theta) * 180 / CV_PI;
                } else if (dx < 0 && dy > 0) {
                        // quadrant II
                        slope = dy / dx;
                        theta = atan(slope);
                        degree = (theta) * 180 / CV_PI + 180;
                } else if (dx < 0 && dy <= 0) {
                        // quadrant III
                        slope = dy / dx;
                        theta = atan(slope);
                        degree = (theta) * 180 / CV_PI + 180;

                } else if (dx > 0 && dy < 0) {
                        // quadrant IV
                        slope = dy / dx;
                        theta = atan(slope);
                        degree = (theta) * 180 / CV_PI + 360;
                }

                sumDegree = sumDegree + degree;
        }

        return sumDegree;

}

void CarCalculations::printText(double ang, IplImage* color_dst) {

        const char* text = "";
        // it is not finished just it is trial and error
        // if angle is between 85 && 95 - go straight
        // <85 - turn right
        // >95 turn left
        if (ang >= 80 && ang <= 97) {
                text = "straight";

        } else if (ang < 80 && ang > 60) {
                text = "left";

        } else if (ang > 45 && ang <= 60) {
                text = "left";
        } else if (ang <= 45) {
                text = "left";
        } else if (ang > 97 && ang <= 120) {
                text = "right";
        } else if (ang < 150 && ang > 120) {
                text = "right";
        } else if (ang < 180 && ang >= 150) {
                text = "right";
        }

        cvPutText(color_dst, text, cvPoint(150, 20), &font1, CV_RGB(255, 255, 255));
}

void CarCalculations::lanedetector()
{
        int call_sequence = 0;
        IplImage* frame = 0;
        CvCapture* capture = 0;
        capture = cvCaptureFromCAM(-1);
        frame = cvQueryFrame(capture);
        if (!capture) {
                fprintf(stderr, "Could not initialize capturing...\n");
        }
        cvNamedWindow("in", 0);
        for (;;) {
                if (!cvGrabFrame(capture))
                        break;
                frame = cvRetrieveFrame(capture);

                // Algorithm!
                IplImage* dst;
                IplImage* color_dst;
                CvMemStorage* storage = cvCreateMemStorage(0);
                CvSeq* lines = 0;

                dst = cvCreateImage(cvGetSize(frame), 8, 1);
                color_dst = cvCreateImage(cvGetSize(frame), 8, 3);

                //Delete the line below in the real algorithm
                cvCvtColor(frame, dst, CV_BGR2GRAY);
                cvCanny(dst, dst, 50, 200, 3);
                cvCvtColor(dst, color_dst, CV_GRAY2BGR);

                // Birds Eye View

                CvPoint2D32f des[4], src[4];
                //Source matrix of the 4 points we need to create the bird eye view
                src[0].x = 160;
                src[0].y = 150;
                src[1].x = 400;
                src[1].y = 150;
                src[2].x = 0;
                src[2].y = 420;
                src[3].x = 600;
                src[3].y = 420;
                //Destination image matrix(usually it'. better to use the maximum limits of the frame)
                des[0].x = 0;
                des[0].y = 0;
                des[1].x = dst-> width - 1;
                des[1].y = 0;
                des[2].x = 0;
                des[2].y = dst-> height - 1;
                des[3].x = dst-> width - 1;
                des[3].y = dst-> height - 1;

                CvMat *H = cvCreateMat(3, 3, CV_32F);
                cvGetPerspectiveTransform(des, src, H);

                IplImage *birds_image = cvCloneImage(dst);
                cvWarpPerspective(dst, birds_image, H, CV_INTER_LINEAR
                                | CV_WARP_INVERSE_MAP | CV_WARP_FILL_OUTLIERS);

                //Edge detection algorithm!!!!
                cvCvtColor(birds_image, color_dst, CV_GRAY2BGR);

                lines = cvHoughLines2(birds_image, storage, CV_HOUGH_PROBABILISTIC, 1,
                                CV_PI / 180, 40, 30, 10);
                for (int i = 0; i < lines->total; i++) {
                        CvPoint* line = (CvPoint*) cvGetSeqElem(lines, i);
                        cvLine(color_dst, line[0], line[1], CV_RGB(255, 255, 255), 3,
                                        CV_AA, 0);
                }

                //end birds eye view

                CvScalar pixelColorRight;
                CvScalar pixelColorLeft;

                // Creating the center vertical line

                int maximumLines = ((color_dst->height * 2) / 6);

                CvPoint* middleBoth = new CvPoint[maximumLines]; // array of a CvPoints
                CvPoint* middleOne = new CvPoint[maximumLines]; // array of a CvPoints

                middleBoth[maximumLines - 1] = cvPoint(0, 0);
                middleOne[maximumLines - 1] = cvPoint(0, 0);

                // storage for Parking
                int indexParking = 0;
                CvPoint* parkingDistance = new CvPoint[maximumLines];
                parkingDistance[maximumLines - 1] = cvPoint(0, 0);

                int indexBoth = 0, indexOne = 0;
                double sumDegreeBoth = 0, sumDegreeOne = 0;

                //Define the variable for the angle of the lane following trajectory

                int pixelsFromTop = 0;

                int countBoth = 0; // countBoth if both sides of line is exist
                int countOne = 0;// countBoth if one sides of line is exist and the other missed
                int countMissedLine = 0;// both line side missed

                CvScalar colorForLines;

                //Fonts Set Up
                initFont();

                //White point x coordinates
                int rightx = 0, leftx = 0;

                //Angle dependency FORMULA NEEDED!!!!!!!!!!!!!!!
                if ((angle < 80 && angle > 60) || (angle > 97 && angle <= 120)) {
                        pixelsFromTop = 300;
                } else if ((angle > 45 && angle <= 60) || (angle < 150 && angle > 120)) {
                        pixelsFromTop = 350;
                } else if ((angle <= 45) || (angle < 180 && angle >= 150)) {
                        pixelsFromTop = 380;
                } else {
                        pixelsFromTop = 200;
                }
                // Create horizontal lines from 480-30=450px until we reach 200px
                for (int y = color_dst->height - 30; y >= pixelsFromTop; y = y - 5) {

                        //Start from the middle-->right and search the first white spot
                        for (int x = color_dst->width / 2; x < color_dst->width - 1; x++) {
                                pixelColorRight = cvGet2D(color_dst, y, x);
                                int whitepixelr = pixelColorRight.val[0];

                                if (whitepixelr == 255) {
                                        rightx = x;
                                        break;
                                } else {
                                        rightx = color_dst->width;
                                }

                        }
                        //Start from the middle-->left and search the first white spot
                        for (int x = color_dst->width / 2; x > 0; x--) {
                                pixelColorLeft = cvGet2D(color_dst, y, x);
                                int whitepixell = pixelColorLeft.val[0];
                                if (whitepixell == 255) {
                                        leftx = x;

                                        break;
                                } else {
                                        leftx = 0;
                                }

                        }
                        // filter the lines -
                        if (leftx == 0 && rightx == color_dst->width) {
                                // nothing if both ends of a line is 0 & 640 - ignore,

                                colorForLines = CV_RGB(255, 0, 0);
                                countMissedLine++;

                        } else if (leftx != 0 && rightx != color_dst->width) {
                                // get center points
                                middleBoth[indexBoth] = cvPoint((int) (rightx + leftx) / 2, y);

                                //get the distance from the center to the right side of line
                                parkingDistance[indexParking] = cvPoint((int) (rightx + leftx)
                                                / 2, y);
                                indexParking++;

                                indexBoth++;
                                countBoth++; // countBoth number of lines
                                colorForLines = CV_RGB(255, 255, 0);
                        } else if (leftx == 0 or rightx == color_dst->width) {
                                // get center points if one of the line is exist
                                middleOne[indexOne] = cvPoint((int) (rightx + leftx) / 2, y);

                                //get the distance from the center to the right side of line
                                parkingDistance[indexParking] = cvPoint((int) (rightx + leftx)
                                                / 2, y);
                                indexParking++;

                                indexOne++;
                                countOne++; // countBoth number of lines
                                colorForLines = CV_RGB(255, 0, 255);
                        }
                        drawline(color_dst, rightx, leftx, y, y, colorForLines);
                }

                if (countBoth > 1) {

                        sumDegreeBoth = calcSumDegree(countBoth, middleBoth);

                        //Average angle
                        angle = sumDegreeBoth / (countBoth - 1);

                        // For Christian
                        char av1[255];
                        sprintf(av1, " %f ", angle);
                        cvPutText(color_dst, av1, cvPoint(90, 20), &font1, CV_RGB(255, 255,
                                        255));
                        cerr << call_sequence << ";" << 0 << ";" << 0 << ";" << 0 << ";"
                                        << angle << endl;
                        call_sequence++;

                        //Print the angle in the top left
                        printText(angle, color_dst);

                } else if (countOne > 1) {

                        sumDegreeOne = calcSumDegree(countOne, middleOne);
                        //  average angle
                        angle = sumDegreeOne / (countOne - 1);

                        //For Christian
                        char av1[255];
                        sprintf(av1, "  %f ", angle);
                        cvPutText(color_dst, av1, cvPoint(90, 20), &font1, CV_RGB(255, 255,
                                        255));
                        cerr << call_sequence << ";" << 0 << ";" << 0 << ";" << 0 << ";"
                                        << angle << endl;
                        call_sequence++;

                        //Print the angle in the top left
                        printText(angle, color_dst);

                } else if (countMissedLine > countBoth && countMissedLine > countOne) {
                        // stop
                        angle = 90;
                        char av1[255];
                        sprintf(av1, " %f ", angle);
                        cvPutText(color_dst, av1, cvPoint(70, 20), &font1, CV_RGB(255, 255,
                                        255));

                        const char* text = "intersection ";
                        cvPutText(color_dst, text, cvPoint(140, 22), &font1, CV_RGB(255,
                                        255, 255));

                }

                cvShowImage("in", color_dst);
                //delay in ms,e.x. 530 gives 2 frames per second, 106 gives about 7 frames per second , 10 gives about 8-9 frames.(For laptop camera)
                cvWaitKey(10);

                cvReleaseImage(&color_dst);
                cvReleaseImage(&dst);
                cvReleaseMemStorage(&storage);
                cvReleaseMat(&H);

        }
        cvReleaseCapture(&capture);
        cvDestroyWindow("in");
}

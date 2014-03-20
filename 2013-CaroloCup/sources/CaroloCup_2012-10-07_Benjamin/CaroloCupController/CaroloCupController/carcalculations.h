#ifndef CARCALCULATIONS_H
#define CARCALCULATIONS_H

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "cv.h"
#include "highgui.h"
#include <iostream>

class CarCalculations
{
public:
    CarCalculations();

private:
    void drawline(IplImage*, int, int, int, int,CvScalar);
    void initFont();
    double calcSumDegree(int , CvPoint* );
    void printText(double,IplImage*);
    void lanedetector();
};

#endif // CARCALCULATIONS_H

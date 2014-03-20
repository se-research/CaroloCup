#ifndef LOCPOINT_H
#define LOCPOINT_H

#include <QPointF>

class LocPoint
{
public:
    LocPoint(double x = 0, double y = 0, double alpha = 0,
             double speed = 0.5, double radius = 0, double sigma = 0);

    double getX();
    double getY();
    double getAlpha();
    double getSpeed();
    QPointF getPoint();
    double getRadius();
    double getSigma();

    void setX(double x);
    void setY(double y);
    void setXY(double x, double y);
    void scaleXY(double scalefactor);
    void setAlpha(double alpha);
    void setSpeed(double speed);
    void setRadius(double radius);
    void setSigma(double sigma);

    // Operators
    LocPoint& operator=(const LocPoint& point);
    bool operator==(const LocPoint& point);
    bool operator!=(const LocPoint& point);

private:
    double mX;
    double mY;
    double mAlpha;
    double mSpeed;
    double mRadius;
    double mSigma;
};

#endif // LOCPOINT_H

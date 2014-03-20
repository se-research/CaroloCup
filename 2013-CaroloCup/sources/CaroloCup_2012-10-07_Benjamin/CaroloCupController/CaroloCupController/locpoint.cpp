#include "locpoint.h"

LocPoint::LocPoint(double x, double y, double alpha, double speed, double radius, double sigma) :
    mX(x), mY(y), mAlpha(alpha), mSpeed(speed), mRadius(radius), mSigma(sigma)
{

}

double LocPoint::getX()
{
    return mX;
}

double LocPoint::getY()
{
    return mY;
}

double LocPoint::getAlpha()
{
    return mAlpha;
}

double LocPoint::getSpeed()
{
    return mSpeed;
}

QPointF LocPoint::getPoint()
{
    return QPointF(mX, mY);
}

double LocPoint::getRadius()
{
    return mRadius;
}

double LocPoint::getSigma()
{
    return mSigma;
}

void LocPoint::setX(double x)
{
    mX = x;
}

void LocPoint::setY(double y)
{
    mY = y;
}

void LocPoint::setXY(double x, double y)
{
    mX = x;
    mY = y;
}

LocPoint &LocPoint::operator =(const LocPoint &point)
{
    mX = point.mX;
    mY = point.mY;
    mAlpha = point.mAlpha;
    mSpeed = point.mSpeed;
    mRadius = point.mRadius;
    mSigma = point.mSigma;
    return *this;
}

bool LocPoint::operator ==(const LocPoint &point)
{
    if (    mX == point.mX &&
            mY == point.mY &&
            mAlpha == point.mAlpha &&
            mSpeed == point.mSpeed &&
            mRadius == point.mRadius &&
            mSigma == point.mSigma) {
        return true;
    } else {
        return false;
    }
}

bool LocPoint::operator !=(const LocPoint &point)
{
    return !(operator==(point));
}

void LocPoint::setAlpha(double alpha)
{
    mAlpha = alpha;
}

void LocPoint::setSpeed(double speed)
{
    mSpeed = speed;
}

void LocPoint::setRadius(double radius)
{
    mRadius = radius;
}

void LocPoint::setSigma(double sigma)
{
    mSigma = sigma;
}

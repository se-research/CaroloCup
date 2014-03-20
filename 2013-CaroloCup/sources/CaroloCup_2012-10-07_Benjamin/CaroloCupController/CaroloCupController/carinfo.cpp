#include "carinfo.h"

CarInfo::CarInfo(QString id, Qt::GlobalColor color)
{
    mId = id;
    mColor = color;
}

QString CarInfo::getId()
{
    return mId;
}

void CarInfo::setId(QString id)
{
    mId = id;
}

void CarInfo::setLocation(LocPoint &point)
{
    mLocation = point;
}

Qt::GlobalColor CarInfo::getColor()
{
    return mColor;
}

void CarInfo::setColor(Qt::GlobalColor color)
{
    mColor = color;
}

LocPoint CarInfo::getLocation()
{
    return mLocation;
}


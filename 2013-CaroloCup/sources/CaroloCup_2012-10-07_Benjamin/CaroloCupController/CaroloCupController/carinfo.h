#ifndef CARINFO_H
#define CARINFO_H

#define QT_NO_KEYWORDS
#include <QVector>
#include <QString>
#include "locpoint.h"

class CarInfo
{
public:
    CarInfo(QString id = "NewCar", Qt::GlobalColor color = Qt::green);
    QString getId();
    void setId(QString id);
    void setLocation(LocPoint &point);
    LocPoint getLocation();
    Qt::GlobalColor getColor();
    void setColor(Qt::GlobalColor color);

private:
    QString mId;
    LocPoint mLocation;
    Qt::GlobalColor mColor;
};

#endif // CARINFO_H

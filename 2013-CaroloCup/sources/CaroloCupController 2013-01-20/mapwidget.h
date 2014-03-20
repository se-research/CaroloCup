#ifndef MAPWIDGET_H
#define MAPWIDGET_H

#include <QGLWidget>
#include <QWidget>
#include <QPainter>
#include <QPaintEvent>
#include <QBrush>
#include <QFont>
#include <QPen>
#include <QPalette>
#include <QVector>
#include <QInputDialog>

#include "locpoint.h"
#include "carinfo.h"

// QWidget or QGLWidget
#define MapWidgetType   QGLWidget

class MapWidget : public MapWidgetType
{
    Q_OBJECT
public:
    explicit MapWidget(QWidget *parent = 0);
    CarInfo* getCarInfo(int car);
    void setFollowCar(int car);
    void addCar(CarInfo car);

    void setScaleFactor(double scale);
    void setRotation(double rotation);
    void setXOffset(double offset);
    void setYOffset(double offset);

Q_SIGNALS:
    void scaleChanged(double newScale);
    void offsetChanged(double newXOffset, double newYOffset);

public Q_SLOTS:

protected:
    void paintEvent(QPaintEvent *event);
    void mouseMoveEvent (QMouseEvent * e);
    void mouseReleaseEvent(QMouseEvent * e);
    void wheelEvent(QWheelEvent * e);

private:
    QVector<CarInfo> mCarInfo;
    double mScaleFactor;
    double mRotation;
    double mXOffset;
    double mYOffset;
    int mMouseLastX;
    int mMouseLastY;
    int mFollowCar;
    double xRealPos;
    double yRealPos;
};

#endif // MAPWIDGET_H

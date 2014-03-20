#include <QDebug>
#include <math.h>
#include <qmath.h>

#include "mapwidget.h"

#include <iostream>
#include <fstream>

MapWidget::MapWidget(QWidget *parent) :
    MapWidgetType(parent)
{
    mScaleFactor = 0.05;
    mRotation = 0;
    mXOffset = 0;
    mYOffset = 0;
    mMouseLastX = 1000000;
    mMouseLastY = 1000000;
    mFollowCar = -1;
    xRealPos = 0;
    yRealPos = 0;
    mCarInfo.clear();
}

CarInfo *MapWidget::getCarInfo(int car)
{
    if (car < mCarInfo.size()) {
        return &mCarInfo[car];
    } else {
        return 0;
    }
}

void MapWidget::addCar(CarInfo car)
{
    mCarInfo.append(car);
    repaint();
}

void MapWidget::setScaleFactor(double scale)
{
    double scaleDiff = scale / mScaleFactor;
    mScaleFactor = scale;
    mXOffset *= scaleDiff;
    mYOffset *= scaleDiff;
    repaint();
}

void MapWidget::setRotation(double rotation)
{
    mRotation = rotation;
    repaint();
}

void MapWidget::setXOffset(double offset)
{
    mXOffset = offset;
    repaint();
}

void MapWidget::setYOffset(double offset)
{
    mYOffset = offset;
    repaint();
}

void MapWidget::setFollowCar(int car)
{
    mFollowCar = car;
    repaint();
}

void MapWidget::paintEvent(QPaintEvent *event)
{
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    const double scaleMax = 5;
    const double scaleMin = 0.0001;
    const double offsetViewMaxFact = 8;

    // Make sure scale and offsetappend is reasonable
    if (mScaleFactor < scaleMin)
    {
        double scaleDiff = scaleMin / mScaleFactor;
        mScaleFactor = scaleMin;
        mXOffset *= scaleDiff;
        mYOffset *= scaleDiff;
    } else if (mScaleFactor > scaleMax)
    {
        double scaleDiff = scaleMax / mScaleFactor;
        mScaleFactor = scaleMax;
        mXOffset *= scaleDiff;
        mYOffset *= scaleDiff;
    }

    if (mXOffset < -(width() * offsetViewMaxFact))
    {
        mXOffset = -(width() * offsetViewMaxFact);
    } else if (mXOffset > (width() * offsetViewMaxFact))
    {
        mXOffset = (width() * offsetViewMaxFact);
    }

    if (mYOffset < -(height() * offsetViewMaxFact))
    {
        mYOffset = -(height() * offsetViewMaxFact);
    } else if (mYOffset > (height() * offsetViewMaxFact))
    {
        mYOffset = (height() * offsetViewMaxFact);
    }

    if (mFollowCar >= 0)
    {
        if (mCarInfo.size() > mFollowCar) {
            LocPoint followLoc = mCarInfo[mFollowCar].getLocation();
            mXOffset = -followLoc.getX() * mScaleFactor;
            mYOffset = -followLoc.getY() * mScaleFactor;
        }
    }

    // Paint begins here
    painter.fillRect(event->rect(), QBrush(Qt::transparent));

    const double car_w = 500;
    const double car_h = 300;
    const double car_corner = 40;
    double angle, x, y;
    QString txt;
    QPointF pt_txt;
    QRectF rect_txt;
    QPen pen;
    QFont font = this->font();

    // Map coordinate transforms
    QTransform drawTrans;
    drawTrans.translate(event->rect().width() / 2 + mXOffset, event->rect().height() / 2 - mYOffset);
    drawTrans.scale( mScaleFactor, -mScaleFactor );
    drawTrans.rotate(mRotation);

    // Text coordinates
    QTransform txtTrans;
    txtTrans.translate(0, 0);
    txtTrans.scale( 1, 1 );
    txtTrans.rotate(0);

    // Set font
    font.setPointSize(10);
    painter.setFont(font);

    // Axis parameters
    const double scaleMult = ceil((1 / ((mScaleFactor * 100) / 50)));
    const double step = 100 * scaleMult;
    const double zeroAxisWidth = 3;
    const QColor zeroAxisColor = Qt::red;
    const QColor firstAxisColor = Qt::gray;
    const QColor secondAxisColor = Qt::blue;
    const QColor textColor = QPalette::Foreground;
    const double xStepFact = ceil(width() * offsetViewMaxFact / step / mScaleFactor);
    const double yStepFact = ceil(height() * offsetViewMaxFact / step / mScaleFactor);

    // Draw Y-axis segments
    for (double i = -xStepFact * step;i < xStepFact * step;i += step)
    {
        if ((int)(i / step) % 2) {
            painter.setPen(firstAxisColor);
        } else {
            txt.sprintf("%.0f", i);
            pt_txt.setX(i);
            pt_txt.setY(0);
            painter.setTransform(txtTrans);
            pt_txt = drawTrans.map(pt_txt);
            pt_txt.setX(pt_txt.x() + 5);
            pt_txt.setY(height() - 10);
            painter.setPen(QPen(textColor));
            painter.drawText(pt_txt, txt);

            if (i == 0)
            {
                pen.setWidthF(zeroAxisWidth / mScaleFactor);
                pen.setColor(zeroAxisColor);
            } else
            {
                pen.setWidth(0);
                pen.setColor(secondAxisColor);
            }
            painter.setPen(pen);
        }

        painter.setTransform(drawTrans);
        painter.drawLine(i, -yStepFact * step, i, yStepFact * step);
    }

    // Draw X-axis segments
    for (double i = -yStepFact * step;i < yStepFact * step;i += step)
    {
        if ((int)(i / step) % 2) {
            painter.setPen(firstAxisColor);
        } else {
            txt.sprintf("%.0f", i);
            pt_txt.setY(i);
            painter.setTransform(txtTrans);
            pt_txt = drawTrans.map(pt_txt);
            pt_txt.setX(10);
            pt_txt.setY(pt_txt.y() - 5);
            painter.setPen(QPen(textColor));
            painter.drawText(pt_txt, txt);

            if (i == 0)
            {
                pen.setWidthF(zeroAxisWidth / mScaleFactor);
                pen.setColor(zeroAxisColor);
            } else
            {
                pen.setWidth(0);
                pen.setColor(secondAxisColor);
            }
            painter.setPen(pen);
        }
        painter.setTransform(drawTrans);
        painter.drawLine(-xStepFact * step, i, xStepFact * step, i);
    }

    // Draw cars
    painter.setPen(QPen(textColor));
    for(QVector<CarInfo>::Iterator it_car = mCarInfo.begin();it_car < mCarInfo.end();it_car++) {
        CarInfo *carInfo = it_car;
        LocPoint pos = carInfo->getLocation();
        x = pos.getX();
        y = pos.getY();
        angle = pos.getAlpha() * 180.0 / M_PI;
        painter.setTransform(drawTrans);

        // Draw standard deviation
        QColor col = Qt::red;
        col.setAlphaF(0.2);
        painter.setBrush(QBrush(col));
        painter.drawEllipse(pos.getPoint(), pos.getSigma(), pos.getSigma());

        // Draw car
        painter.setBrush(QBrush(Qt::black));
        painter.save();
        painter.translate(x, y);
        painter.rotate(angle);
        // Wheels
        painter.drawRoundedRect(-75,-150, 150, car_h, car_corner-10, car_corner-10);
        painter.drawRoundedRect(205,-150, 150, car_h, car_corner-10, car_corner-10);
        // Front bumper
        painter.setBrush(QBrush(Qt::green));
        painter.drawRoundedRect(-95, -130, car_w, car_h-40, car_corner, car_corner);
        // Hull
        painter.setBrush(QBrush(carInfo->getColor()));
        painter.drawRoundedRect(-95,-130, car_w-40, car_h-40, car_corner, car_corner);
        painter.restore();
        // Center
        painter.setBrush(QBrush(Qt::red));
        painter.drawEllipse(QPoint(x, y), 20, 20);

        // Print data
        txt.sprintf("%s\n(%.0f, %.0f, %.0f)", carInfo->getId().toAscii().data(),
                    x, y, angle);
        pt_txt.setX(x + 120 + (car_w - 190) * ((cos(pos.getAlpha()) + 1) / 2));
        pt_txt.setY(y);
        painter.setTransform(txtTrans);
        pt_txt = drawTrans.map(pt_txt);
        rect_txt.setCoords(pt_txt.x(), pt_txt.y() - 20,
                           pt_txt.x() + 150, pt_txt.y() + 25);
        painter.drawText(rect_txt, txt);
    }

    // Draw units (mm)
    painter.setTransform(txtTrans);
    font.setPointSize(16);
    painter.setFont(font);
    txt = "(mm)";
    painter.drawText(width() - 50, 35, txt);

    painter.end();
}

void MapWidget::mouseMoveEvent(QMouseEvent *e)
{
    if (e->buttons() & Qt::LeftButton)
    {
        int x = e->pos().x();
        int y = e->pos().y();

        if (mMouseLastX < 100000)
        {
            int diffx = x - mMouseLastX;
            mXOffset += diffx;
            repaint();
        }

        if (mMouseLastY < 100000)
        {
            int diffy = y - mMouseLastY;
            mYOffset -= diffy;

            Q_EMIT(offsetChanged(mXOffset, mYOffset));
            repaint();
        }

        mMouseLastX = x;
        mMouseLastY = y;
    }
}

void MapWidget::mouseReleaseEvent(QMouseEvent *e)
{
    if (!(e->buttons() & Qt::LeftButton))
    {
        mMouseLastX = 1000000;
        mMouseLastY = 1000000;
    }
}

void MapWidget::wheelEvent(QWheelEvent *e)
{
    int x = e->pos().x();
    int y = e->pos().y();
    double scaleDiff = ((double)e->delta() / 600.0);
    if (scaleDiff > 0.8)
    {
        scaleDiff = 0.8;
    }

    if (scaleDiff < -0.8)
    {
        scaleDiff = -0.8;
    }

    x -= width() / 2;
    y -= height() / 2;

    mScaleFactor += mScaleFactor * scaleDiff;
    mXOffset += mXOffset * scaleDiff;
    mYOffset += mYOffset * scaleDiff;

    Q_EMIT(scaleChanged(mScaleFactor));
    Q_EMIT(offsetChanged(mXOffset, mYOffset));
    repaint();
}

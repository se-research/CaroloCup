/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>
#include <string>

#include "core/base/Lock.h"

#include "plugins/surroundingsviewer/SurroundingsViewerRenderer2D.h"

namespace plugins {
    namespace surroundingsviewer {

        using namespace std;
        using namespace core::base;
        using namespace hesperia::data::environment;

        SurroundingsViewerRenderer2D::SurroundingsViewerRenderer2D() :
            m_rendererMutex(),
            m_painter(NULL),
            m_rect(),
            m_pen() {}

        SurroundingsViewerRenderer2D::~SurroundingsViewerRenderer2D() {}

        void SurroundingsViewerRenderer2D::update(QPainter *painter, const QRectF &rect) {
            Lock l(m_rendererMutex);
            m_painter = painter;
            m_rect = rect;
        }

        void SurroundingsViewerRenderer2D::setColor(const Point3 &c) {
            m_pen.setColor(QColor(static_cast<int>(c.getX()*255), static_cast<int>(c.getY()*255), static_cast<int>(c.getZ()*255), 255));
        }

        void SurroundingsViewerRenderer2D::setPointWidth(const double &width) {
            m_pen.setWidth(static_cast<int>(width));
        }

        void SurroundingsViewerRenderer2D::setLineWidth(const double &width) {
            m_pen.setWidth(static_cast<int>(width));
        }

        void SurroundingsViewerRenderer2D::drawText(const Point3 &p, const string &text) {
            Lock l(m_rendererMutex);
            if (m_painter != NULL) {
                m_painter->setPen(m_pen);
                m_painter->drawText(static_cast<int>(p.getX()), static_cast<int>(m_rect.height() - p.getY()), QString(text.c_str()));
            }
        }

        void SurroundingsViewerRenderer2D::drawPoint(const Point3 &p) {
            Lock l(m_rendererMutex);
            if (m_painter != NULL) {
                m_painter->setPen(m_pen);
                m_painter->drawPoint(static_cast<int>(p.getX()), static_cast<int>(m_rect.height() - p.getY()));
            }
        }

        void SurroundingsViewerRenderer2D::drawLine(const Point3 &A, const Point3 &B) {
            Lock l(m_rendererMutex);
            if (m_painter != NULL) {
                m_painter->setPen(m_pen);
                m_painter->drawLine(static_cast<int>(A.getX()), static_cast<int>(m_rect.height() - A.getY()), static_cast<int>(B.getX()), static_cast<int>(m_rect.height() - B.getY()));
            }
        }

        void SurroundingsViewerRenderer2D::drawImage(const core::wrapper::Image *image, const Point3 &originPixelXY, const Point3 &scalingPixelXY, const float &/*rotationZ*/) {
            Lock l(m_rendererMutex);
            if (m_painter != NULL) {
                if (image != NULL) {
                    // Why the heck do we have to add +1 to the rows???
                    QImage qi((const uchar*)image->getRawData(), image->getWidth()+1, image->getHeight()-3, (image->getWidth()+1) * 3, QImage::Format_RGB888);

                    qi = qi.rgbSwapped();
                    qi = qi.scaled(static_cast<int>(scalingPixelXY.getX() * (image->getWidth()+1)), static_cast<int>(scalingPixelXY.getY() * (image->getHeight()-3)));

                    Point3 translate;
                    translate.setX(-1 * originPixelXY.getX() * scalingPixelXY.getX());
                    translate.setY(originPixelXY.getY() * scalingPixelXY.getY());

                    m_painter->drawImage(static_cast<int>(translate.getX()), static_cast<int>(m_rect.height() - translate.getY()), qi);
                }
            }
        }

    }
} // plugins::surroundingsviewer

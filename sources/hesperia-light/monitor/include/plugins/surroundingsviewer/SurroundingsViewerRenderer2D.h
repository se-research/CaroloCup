/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERRENDERER2D_H_
#define PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERRENDERER2D_H_

#include "core/base/Mutex.h"
#include "hesperia/decorator/twoD/Renderer2D.h"

#include "QtIncludes.h"

namespace plugins {
    namespace surroundingsviewer {

        using namespace std;

        /**
         * This class is the actual renderer.
         */
        class SurroundingsViewerRenderer2D : public hesperia::decorator::twoD::Renderer2D {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SurroundingsViewerRenderer2D(const SurroundingsViewerRenderer2D &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SurroundingsViewerRenderer2D& operator=(const SurroundingsViewerRenderer2D &/*obj*/);

            public:
                SurroundingsViewerRenderer2D();

                virtual ~SurroundingsViewerRenderer2D();

                /**
                 * This method updates the renderer.
                 *
                 * @param painter Painter to be used.
                 * @param rect Rectangular viewport to swap direction of the Y-axis.
                 */
                void update(QPainter *painter, const QRectF &rect);

            protected:
                virtual void setColor(const hesperia::data::environment::Point3 &c);

                virtual void setPointWidth(const double &width);

                virtual void setLineWidth(const double &width);

                virtual void drawText(const hesperia::data::environment::Point3 &p, const string &text);

                virtual void drawPoint(const hesperia::data::environment::Point3 &p);

                virtual void drawLine(const hesperia::data::environment::Point3 &A, const hesperia::data::environment::Point3 &B);

                virtual void drawImage(const core::wrapper::Image *image, const hesperia::data::environment::Point3 &originPixelXY, const hesperia::data::environment::Point3 &scalingPixelXY, const float &rotationZ);

            private:
                core::base::Mutex m_rendererMutex;
                QPainter *m_painter;
                QRectF m_rect;
                QPen m_pen;
        };

    }
} // plugins::surroundingsviewer

#endif /*PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERRENDERER2D_H_*/

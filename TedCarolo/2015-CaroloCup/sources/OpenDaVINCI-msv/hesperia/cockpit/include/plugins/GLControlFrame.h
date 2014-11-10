/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef COCKPIT_GLCONTROLFRAME_H_
#define COCKPIT_GLCONTROLFRAME_H_

#include "QtIncludes.h"

#include "plugins/AbstractGLWidget.h"

namespace cockpit {
    namespace plugins {

        class GLControlFrame: public QWidget {

                Q_OBJECT

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                GLControlFrame(const GLControlFrame &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                GLControlFrame& operator=(const GLControlFrame &/*obj*/);

            public:
                GLControlFrame(AbstractGLWidget* controllableGLWidget);

                virtual ~GLControlFrame();

            signals:
                void horizontalValueChanged(double newValue);
                void verticalValueChanged(double newValue);

            private:
                QwtWheel* m_hWheel;
                QwtWheel* m_vWheel;

            private slots:
                void newVValue(double newValue);
                void newHValue(double newValue);
                void resetHValue();
                void resetVValue();
        };
    }
}

#endif /* COCKPIT_GLCONTROLFRAME_H_ */

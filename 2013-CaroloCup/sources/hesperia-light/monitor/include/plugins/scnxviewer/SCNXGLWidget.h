/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MONITOR_PLUGINS_SCNXVIEWER_SCNXGLWIDGET_H_
#define MONITOR_PLUGINS_SCNXVIEWER_SCNXGLWIDGET_H_

#include <string>

#include "core/base/KeyValueConfiguration.h"
#include "core/base/Mutex.h"
#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/TransformGroup.h"

#include "plugins/AbstractGLWidget.h"

namespace plugins {
    namespace scnxviewer {

        using namespace std;

        /**
         * This class visualizes scnx models.
         */
        class SCNXGLWidget: public AbstractGLWidget {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SCNXGLWidget(const SCNXGLWidget &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SCNXGLWidget& operator=(const SCNXGLWidget &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param plugIn Reference to the plugin to which this widget belongs.
                 * @param prnt Pointer to the parental widget.
                 */
                SCNXGLWidget(const PlugIn &plugIn, QWidget *prnt);

                virtual ~SCNXGLWidget();

                /**
                 * This method sets a new scnx-model to be loaded.
                 *
                 * @param scnxModel Model.
                 */
                void setSCNXModel(const string &scnxModel);

            protected:
                virtual void setupOpenGL();

                virtual void initScene();

                virtual void drawScene();

            private:
                hesperia::threeD::Node *m_root;

                mutable core::base::Mutex m_modelMutex;
                hesperia::threeD::TransformGroup *m_model;
        };

    }
} // plugins::scnxviewer

#endif /*MONITOR_PLUGINS_SCNXVIEWER_SCNXGLWIDGET_H_*/

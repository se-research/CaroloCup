/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERGLWIDGET_H_
#define PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERGLWIDGET_H_

#include <map>

#include "core/base/Mutex.h"

#include "core/data/Container.h"
#include "hesperia/data/scenario/Scenario.h"
#include "hesperia/decorator/DataRenderer.h"
#include "hesperia/decorator/ScenarioRenderer.h"
#include "hesperia/decorator/threeD/Renderer3D.h"

#include "plugins/AbstractGLWidget.h"

namespace plugins {
    namespace surroundingsviewer {

        /**
         * This class is the viewport for a 3D scene.
         */
        class SurroundingsViewerGLWidget : public AbstractGLWidget, public core::io::ContainerListener {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SurroundingsViewerGLWidget(const SurroundingsViewerGLWidget &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SurroundingsViewerGLWidget& operator=(const SurroundingsViewerGLWidget &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param plugIn Reference to the plugin to which this widget belongs.
                 * @param dr DataRenderer.
                 * @param sr ScenarioRenderer.
                 * @param s Scenario.
                 * @param prnt Pointer to the parental widget.
                 */
                SurroundingsViewerGLWidget(const PlugIn &plugIn, hesperia::decorator::DataRenderer *dr, hesperia::decorator::ScenarioRenderer *sr, hesperia::data::scenario::Scenario *s, QWidget *prnt);

                virtual ~SurroundingsViewerGLWidget();

                virtual void nextContainer(core::data::Container &c);

                /**
                 * This method is called to assign the camera to EgoState.
                 *
                 * @param b True, if the camera is assigned to follow EgoState.
                 */
                void assignToEgoState(const bool &b);

            protected:
                virtual void setupOpenGL();

                virtual void initScene();

                virtual void drawScene();

            private:
                core::base::Mutex m_drawMutex;

                hesperia::decorator::DataRenderer *m_dataRenderer;

                hesperia::decorator::ScenarioRenderer *m_scenarioRenderer;
                hesperia::data::scenario::Scenario *m_scenario;

                hesperia::decorator::threeD::Renderer3D m_renderer3D;

                map<core::data::Container::DATATYPE, core::data::Container> m_drawMap;

                bool m_cameraFollowsEgoState;

                /**
                 * This method renders the scene.
                 */
                void renderTheScene();
        };

    }
} // plugins::surroundingsviewer

#endif /*PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERGLWIDGET_H_*/

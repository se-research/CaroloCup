/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERWIDGET_H_
#define PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERWIDGET_H_

#include "hesperia/data/scenario/Scenario.h"
#include "hesperia/decorator/DataRenderer.h"
#include "hesperia/decorator/ScenarioRenderer.h"
#include "hesperia/scenario/SCNXArchive.h"

#include "QtIncludes.h"

#include "plugins/GLControlFrame.h"
#include "plugins/surroundingsviewer/SurroundingsViewerGLWidget.h"
#include "plugins/surroundingsviewer/SurroundingsViewerWidget2D.h"

namespace plugins {
    namespace surroundingsviewer {

        /**
         * This class is the outer container for the 3D scene graph viewer
         * implemented in SurroundingsViewerGLWidget and a tree like data
         * structure on its right hand side.
         */
        class SurroundingsViewerWidget : public QWidget, public core::io::ContainerListener {

            Q_OBJECT

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SurroundingsViewerWidget(const SurroundingsViewerWidget &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SurroundingsViewerWidget& operator=(const SurroundingsViewerWidget &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param plugIn Reference to the plugin to which this widget belongs.
                 * @param prnt Pointer to the parental widget.
                 */
                SurroundingsViewerWidget(const PlugIn &plugIn, QWidget *prnt);

                virtual ~SurroundingsViewerWidget();

                virtual void nextContainer(core::data::Container &c);

            private slots:
                /**
                 * This method is called whenever an item in the combobox
                 * for the color of the background changes its state.
                 *
                 * @param item Item that changed.
                 */
                void selectedItemChangedBackground(const QString &item);

                /**
                 * This method is called whenever an item in the combobox
                 * for the assignable camera changes its state.
                 *
                 * @param item Item that changed.
                 */
                void selectedItemChangedCamera(const QString &item);

            private:
                GLControlFrame *m_viewerControlWidget;
                SurroundingsViewerGLWidget *m_surroundingsViewerGLWidget;
                SurroundingsViewerWidget2D *m_surroundingsViewerWidget2D;

                QComboBox *m_cameraSelector;
                QComboBox *m_backgroundSelector;

                hesperia::decorator::DataRenderer m_dataRendererWithCarModel;
                hesperia::decorator::DataRenderer m_dataRenderer;

                hesperia::scenario::SCNXArchive *m_scnxArchive;
                hesperia::data::scenario::Scenario *m_scenario;
                hesperia::decorator::ScenarioRenderer m_scenarioRenderer;

                QGraphicsScene *m_scene;
                QGraphicsView *m_view;
        };

    }
} // plugins::surroundingsviewer

#endif /*PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERWIDGET_H_*/

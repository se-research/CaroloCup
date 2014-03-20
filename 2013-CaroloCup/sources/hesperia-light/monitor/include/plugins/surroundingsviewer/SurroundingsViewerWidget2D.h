/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERWIDGET2D_H_
#define PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERWIDGET2D_H_

#include <map>

#include "core/base/Mutex.h"
#include "core/data/Container.h"
#include "core/io/ContainerListener.h"

#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/scenario/Scenario.h"
#include "hesperia/decorator/DataRenderer.h"
#include "hesperia/decorator/ScenarioRenderer.h"

#include "QtIncludes.h"
#include "plugins/surroundingsviewer/SurroundingsViewerRenderer2D.h"

namespace plugins {
    namespace surroundingsviewer {

        using namespace std;

        /**
         * This class is the context for drawing two-dimensionally.
         */
        class SurroundingsViewerWidget2D : public QGraphicsItem, public core::io::ContainerListener {

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SurroundingsViewerWidget2D(const SurroundingsViewerWidget2D &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SurroundingsViewerWidget2D& operator=(const SurroundingsViewerWidget2D &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param dr DataRenderer.
                 * @param sr ScenarioRenderer.
                 * @param s Scenario.
                 */
                SurroundingsViewerWidget2D(hesperia::decorator::DataRenderer *dr, hesperia::decorator::ScenarioRenderer *sr, hesperia::data::scenario::Scenario *s);

                virtual ~SurroundingsViewerWidget2D();

                virtual void nextContainer(core::data::Container &c);

            protected:
                virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

                virtual QRectF boundingRect() const;

            private:
                core::base::Mutex m_drawMutex;
                map<core::data::Container::DATATYPE, core::data::Container> m_drawMap;

                hesperia::decorator::DataRenderer *m_dataRenderer;
                hesperia::decorator::ScenarioRenderer *m_scenarioRenderer;
                hesperia::data::scenario::Scenario *m_scenario;

                SurroundingsViewerRenderer2D m_renderer2D;
        };

    }
} // plugins::surroundingsviewer

#endif /*PLUGINS_SURROUNDINGSVIEWER_SURROUNDINGSVIEWERWIDGET2D_H_*/

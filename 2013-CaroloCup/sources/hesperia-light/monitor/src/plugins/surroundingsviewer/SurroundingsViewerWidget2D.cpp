/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Lock.h"

#include "hesperia/data/environment/EgoState.h"

#include "QtIncludes.h"

#include "plugins/surroundingsviewer/SurroundingsViewerWidget2D.h"

namespace plugins {
    namespace surroundingsviewer {

        using namespace std;
        using namespace core::base;
        using namespace core::data;
        using namespace hesperia::data::environment;
        using namespace hesperia::data::scenario;
        using namespace hesperia::decorator;

        SurroundingsViewerWidget2D::SurroundingsViewerWidget2D(DataRenderer *dr, ScenarioRenderer *sr, Scenario *s) :
            m_drawMutex(),
            m_drawMap(),
            m_dataRenderer(dr),
            m_scenarioRenderer(sr),
            m_scenario(s),
            m_renderer2D() {}

        SurroundingsViewerWidget2D::~SurroundingsViewerWidget2D() {}

        void SurroundingsViewerWidget2D::nextContainer(core::data::Container &c) {
            Lock l(m_drawMutex);
            m_drawMap[c.getDataType()] = c;
        }

        QRectF SurroundingsViewerWidget2D::boundingRect() const {
            qreal penWidth = 1;
            return QRectF(-40 - penWidth / 2, -40 - penWidth / 2, 40 + penWidth / 2, 40 + penWidth / 2);
        }

        void SurroundingsViewerWidget2D::paint(QPainter *painter, const QStyleOptionGraphicsItem * /*option*/, QWidget * /*widget*/) {
            Lock l(m_drawMutex);
            if (painter != NULL) {
                // Delegate paint event.
                m_renderer2D.update(painter, boundingRect());

                // Set renderer.
                if ( (m_scenarioRenderer != NULL) && (m_scenario != NULL) ) {
                    m_scenarioRenderer->setRenderer(&m_renderer2D);
                    m_scenario->accept(*m_scenarioRenderer);
                }

                // Draw surroundings.
                if (m_dataRenderer != NULL) {
                    m_dataRenderer->setRenderer(&m_renderer2D);
                    map<Container::DATATYPE, Container>::iterator it = m_drawMap.begin();
                    while (it != m_drawMap.end()) {
                        m_dataRenderer->draw(it->second);
                        it++;
                    }
                }
            }
        }

    }
} // plugins::surroundingsviewer

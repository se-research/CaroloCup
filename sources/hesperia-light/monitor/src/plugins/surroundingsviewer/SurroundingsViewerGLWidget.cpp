/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"
#include "core/base/Lock.h"

#include "hesperia/data/Constants.h"
#include "hesperia/data/environment/Point3.h"
#include "hesperia/data/environment/Position.h"

#include "plugins/surroundingsviewer/SurroundingsViewerGLWidget.h"

namespace plugins {
    namespace surroundingsviewer {

        using namespace core::base;
        using namespace core::data;
        using namespace hesperia::data;
        using namespace hesperia::data::environment;
        using namespace hesperia::data::scenario;
        using namespace hesperia::decorator;
        using namespace hesperia::decorator::threeD;

        SurroundingsViewerGLWidget::SurroundingsViewerGLWidget(const PlugIn &plugIn, DataRenderer *dr, ScenarioRenderer *sr, Scenario *s, QWidget *prnt) :
                AbstractGLWidget(plugIn, prnt),
                m_drawMutex(),
                m_dataRenderer(dr),
                m_scenarioRenderer(sr),
                m_scenario(s),
                m_renderer3D(),
                m_drawMap(),
                m_cameraFollowsEgoState(false) {}

        SurroundingsViewerGLWidget::~SurroundingsViewerGLWidget() {}

        void SurroundingsViewerGLWidget::initScene() {}

        void SurroundingsViewerGLWidget::setupOpenGL() {
//            glEnable(GL_LIGHTING);
//
//            glEnable(GL_LIGHT0);
//            float light0Position[4] = {0, 0, 20, 0};
//            float light0Ambient[4] = {0.5f, 0.5f, 0.5f, 0};
//            float light0Diffuse[4] = {0.8f, 0.8f, 0.8f, 0};
//            float light0Specular[4] = {0, 0, 0, 0};
//            glLightfv(GL_LIGHT0, GL_POSITION, light0Position);
//            glLightfv(GL_LIGHT0, GL_AMBIENT, light0Ambient);
//            glLightfv(GL_LIGHT0, GL_DIFFUSE, light0Diffuse);
//            glLightfv(GL_LIGHT0, GL_SPECULAR, light0Specular);
        }

        void SurroundingsViewerGLWidget::assignToEgoState(const bool &b) {
            Lock l(m_drawMutex);
            m_cameraFollowsEgoState = b;
        }

        void SurroundingsViewerGLWidget::drawScene() {
            Lock l(m_drawMutex);

            if (m_cameraFollowsEgoState) {
                Position assignedNode;

                map<core::data::Container::DATATYPE, core::data::Container>::iterator result = m_drawMap.find(Container::EGOSTATE);
                if (result != m_drawMap.end()) {
                    assignedNode = result->second.getData<EgoState>();
                }

                Point3 positionCamera;
                Point3 lookAtPointCamera;
                Point3 dirCamera(-10, 0, 0);
                dirCamera.rotateZ(assignedNode.getRotation().getAngleXY());
                positionCamera.setX(assignedNode.getPosition().getX() + dirCamera.getX());
                positionCamera.setY(assignedNode.getPosition().getY() + dirCamera.getY());
                positionCamera.setZ(10);

                lookAtPointCamera.setX(assignedNode.getPosition().getX());
                lookAtPointCamera.setY(assignedNode.getPosition().getY());
                lookAtPointCamera.setZ(0);

                glPushMatrix();
                    glLoadIdentity();

                    // Setup camera.
                    gluLookAt(positionCamera.getX(), positionCamera.getY(), positionCamera.getZ(),
                              lookAtPointCamera.getX(), lookAtPointCamera.getY(), lookAtPointCamera.getZ(),
                              0, 0, 1);

                    // Draw scene.
                    renderTheScene();
                glPopMatrix();
            }
            else {
                renderTheScene();
            }
        }

        void SurroundingsViewerGLWidget::renderTheScene() {
            // Perform 3D rendering.
            if ( (m_scenarioRenderer != NULL) && (m_scenario != NULL) ) {
                m_renderer3D.beginPainting();
                    m_scenarioRenderer->setRenderer(&m_renderer3D);
                    m_scenario->accept(*m_scenarioRenderer);
                m_renderer3D.endPainting();
            }

            // Draw surroundings.
            if (m_dataRenderer != NULL) {
                m_dataRenderer->setRenderer(&m_renderer3D);
                map<Container::DATATYPE, Container>::iterator it = m_drawMap.begin();
                while (it != m_drawMap.end()) {
                    m_dataRenderer->draw(it->second);
                    it++;
                }
            }
        }

        void SurroundingsViewerGLWidget::nextContainer(Container &c) {
            Lock l(m_drawMutex);
            m_drawMap[c.getDataType()] = c;
        }
    }
} // plugins::surroundingsviewer

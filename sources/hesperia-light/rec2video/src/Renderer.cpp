/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <fstream>
#include <sstream>
#include <vector>

#include <GL/gl.h>
#include <GL/glut.h>

#include <opencv/highgui.h>

#include "core/base/Lock.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/io/URL.h"
#include "hesperia/data/environment/Point3.h"
#include "hesperia/data/environment/EgoState.h"
#include "hesperia/data/sensor/ContouredObject.h"
#include "hesperia/data/sensor/ContouredObjects.h"
#include "hesperia/data/scenario/Scenario.h"
#include "hesperia/scenario/SCNXArchive.h"
#include "hesperia/scenario/SCNXArchiveFactory.h"
#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/decorator/DecoratorFactory.h"
#include "hesperia/threeD/loaders/OBJXArchive.h"
#include "hesperia/threeD/loaders/OBJXArchiveFactory.h"
#include "hesperia/threeD/models/Point.h"

#include "Renderer.h"

namespace rec2video {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;
    using namespace hesperia::data;
    using namespace hesperia::data::environment;
    using namespace hesperia::data::scenario;
    using namespace hesperia::data::sensor;
    using namespace hesperia::scenario;
    using namespace hesperia::threeD;
    using namespace hesperia::threeD::decorator;
    using namespace hesperia::threeD::loaders;
    using namespace hesperia::threeD::models;

    Renderer::Renderer(const KeyValueConfiguration &kvc, Condition &condition, bool &doRendering) :
        Service(),
        GLUTWindowBase<Renderer>(this),
        m_kvc(kvc),
        m_condition(condition),
        m_doRendering(doRendering),
        m_image(NULL),
        m_flippedImage(NULL),
        m_imageNumber(0),
        m_positionCamera(),
        m_lookAtPointCamera(),
        m_renderer3D(),
        m_drawMap(),
        m_dataRendererWithCarModel(),
        m_scnxArchive(NULL),
        m_scenario(NULL),
        m_scenarioRenderer() {}

    Renderer::~Renderer() {
        if (m_image != NULL) {
            cvReleaseImage(&m_image);
            m_image = NULL;
        }
        if (m_flippedImage != NULL) {
            cvReleaseImage(&m_flippedImage);
            m_flippedImage = NULL;
        }
    }

    void Renderer::run() {
        // Setup images.
        const int32_t frameW  = 640;
        const int32_t frameH  = 480;
        m_image = cvCreateImage(cvSize(frameW, frameH), IPL_DEPTH_8U, 3);
        m_flippedImage = cvCreateImage(cvSize(frameW, frameH), IPL_DEPTH_8U, 3); // The OpenGL readback produces a horizontally flipped image.

        // Setup GLUT.
        string argv0("rec2video");
        int32_t argc = 1;
        char **argv;
        argv = new char*[1];
        argv[0] = const_cast<char*>(argv0.c_str());

        glutInit(&argc, argv);
        glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGB);
        glutInitWindowPosition(50, 50);
        glutInitWindowSize(frameW, frameH);
        glutCreateWindow("rec2video");

        setupGLUTCallbacks();

        glEnable(GL_DEPTH_TEST);
        glClearColor(0, 0, 0, 0);

        // Load scenario.
        const URL urlOfSCNXFile(m_kvc.getValue<string>("global.scenario"));
        if (urlOfSCNXFile.isValid()) {
            m_scnxArchive = &(SCNXArchiveFactory::getInstance().getSCNXArchive(urlOfSCNXFile));

            if (m_scnxArchive != NULL) {
                m_scenario = &(m_scnxArchive->getScenario());
                m_scenarioRenderer.setSCNXArchive(m_scnxArchive);
            }
        }

        // Try loading the car model.
        m_dataRendererWithCarModel.setEgoStateModel(m_kvc.getValue<string>("global.car"));

        m_dataRendererWithCarModel.setSCNXArchive(m_scnxArchive);

        // Renderer ready.
        serviceReady();
        glutMainLoop();
    }

    void Renderer::process(Container &c) {
        m_drawMap[c.getDataType()] = c;

        // Get data for following the EgoState.
        if (c.getDataType() == Container::EGOSTATE) {
            EgoState egostate = c.getData<EgoState>();
            Point3 dirCamera(-10, 0, 0);
            dirCamera.rotateZ(egostate.getRotation().getAngleXY());
            m_positionCamera.setX(egostate.getPosition().getX() + dirCamera.getX());
            m_positionCamera.setY(egostate.getPosition().getY() + dirCamera.getY());
            m_positionCamera.setZ(10);

            m_lookAtPointCamera.setX(egostate.getPosition().getX());
            m_lookAtPointCamera.setY(egostate.getPosition().getY());
            m_lookAtPointCamera.setZ(0);
        }
    }

    void Renderer::render() {
        if (isRunning()) {
            // Wait for rendering signal triggered by Rec2Video.
            Lock l(m_condition);
            while (!m_doRendering) {
                m_condition.waitOnSignal();
            }

            // Do render only once.
            m_doRendering = false;

            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
            glMatrixMode(GL_MODELVIEW);

            glPushMatrix();
                glLoadIdentity();

                // Setup camera.
                gluLookAt(m_positionCamera.getX(), m_positionCamera.getY(), m_positionCamera.getZ(),
                          m_lookAtPointCamera.getX(), m_lookAtPointCamera.getY(), m_lookAtPointCamera.getZ(),
                          0, 0, 1);

                // Perform 3D rendering.
                if (m_scenario != NULL) {
                    m_renderer3D.beginPainting();
                        m_scenarioRenderer.setRenderer(&m_renderer3D);
                        m_scenario->accept(m_scenarioRenderer);
                    m_renderer3D.endPainting();
                }

                // Draw surroundings.
                m_dataRendererWithCarModel.setRenderer(&m_renderer3D);
                map<Container::DATATYPE, Container>::iterator it = m_drawMap.begin();
                while (it != m_drawMap.end()) {
                    m_dataRendererWithCarModel.draw(it->second);
                    it++;
                }
            glPopMatrix();

            glFlush();

            if ( (m_image != NULL) && (m_flippedImage != NULL) ) {
                glReadBuffer(GL_BACK);
                glPixelStorei(GL_PACK_ALIGNMENT, 1);
                glReadPixels(0, 0, m_flippedImage->width, m_flippedImage->height, GL_BGR, GL_UNSIGNED_BYTE, m_flippedImage->imageData);

                cvConvertImage(m_flippedImage, m_image, CV_CVTIMG_FLIP);

                stringstream sstr;
                sstr << "image-";
                sstr.fill('0');
                sstr.width(8);
                sstr << m_imageNumber++;
                sstr.width(0);
                sstr << ".png";
                cvSaveImage(sstr.str().c_str(), m_image);
            }
        }
    }

    void Renderer::resize(int32_t w, int32_t h) {
        h = (h == 0) ? 1 : h;
        glViewport(0, 0, w, h);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        gluPerspective(60, w/h, 1, 1000);
        glMatrixMode(GL_MODELVIEW);
        glutPostRedisplay();
    }

    void Renderer::beforeStop() {}

    void Renderer::doIdle() {}

    void Renderer::processMouseMotion(int32_t /*x*/, int32_t /*y*/) {}

    void Renderer::processMouseEvent(int32_t /*button*/, int32_t /*state*/, int32_t /*x*/, int32_t /*y*/) {}

    void Renderer::processKey(unsigned char /*key*/, int32_t /*x*/, int32_t /*y*/) {}


} // rec2video

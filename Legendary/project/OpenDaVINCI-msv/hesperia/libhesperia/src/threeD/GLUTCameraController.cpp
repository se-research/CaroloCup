#include "hesperia/threeD/GLUTCameraController.h"

namespace hesperia {
    namespace threeD {

       GLUTCameraController::GLUTCameraController(hesperia::threeD::Camera& camera,
                                                  const double positionDelta,
                                                  const double orientationDelta) :
           m_camera(camera),
           m_positionDelta(positionDelta),
           m_orientationDelta(orientationDelta),
           m_mouseX(0),
           m_mouseY(0),
           m_mouseButton(0)
       {}

       GLUTCameraController::~GLUTCameraController()
       {}

       void GLUTCameraController::processKey(unsigned char key, int32_t /*x*/, int32_t /*y*/) {
           if (key == 'w') {
               m_camera.moveFoward(m_positionDelta);
           }
           if (key == 's') {
               m_camera.moveBackward(m_positionDelta);
           }
           if (key == 'a') {
               m_camera.strafeLeft(m_positionDelta);
           }
           if (key == 'd') {
               m_camera.strafeRight(m_positionDelta);
           }
       }

       void GLUTCameraController::processMouseMotion(int32_t x, int32_t y) {
           // Left mouse button pressed.
           if (m_mouseButton == GLUT_LEFT_BUTTON) {
               m_camera.changePitch(m_orientationDelta * (m_mouseY - y) );
               m_camera.changeHead( m_orientationDelta * (m_mouseX - x) );
           }

           // Save mouse position for later usage.
           m_mouseX = x;
           m_mouseY = y;
       }

       void GLUTCameraController::processMouseEvent(int32_t button, int32_t state, int32_t x, int32_t y) {
           if (state == GLUT_DOWN) {
               m_mouseButton = button;
               m_mouseX = x;
               m_mouseY = y;
           }
           else {
               m_mouseButton = 0;
           }
       }
    }
}


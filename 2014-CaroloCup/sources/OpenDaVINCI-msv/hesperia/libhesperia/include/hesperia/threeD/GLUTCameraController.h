/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_THREED_GLUTCAMERACONTROLLER_H_
#define HESPERIA_THREED_GLUTCAMERACONTROLLER_H_

#include <GL/gl.h>
#include <GL/glut.h>

#include "hesperia/threeD/Camera.h"

namespace hesperia {
    namespace threeD {

        /**
         * Eases the use of a Camera in a GLUTWindow. Simply call the process... methods
         * and can control the camera like a First-Person-Shooter.
         */
        class GLUTCameraController
        {
            public:
                GLUTCameraController(hesperia::threeD::Camera& camera,
                                     double positionDelta = 0.5,
                                     double orientationDelta = 0.08 );

                virtual ~GLUTCameraController();

                void processKey(unsigned char key, int32_t x, int32_t y);
                void processMouseMotion(int32_t x, int32_t y);
                void processMouseEvent(int32_t button, int32_t state, int32_t x, int32_t y);

            protected:
                hesperia::threeD::Camera& m_camera;
                double m_positionDelta;
                double m_orientationDelta;
                int32_t m_mouseX;
                int32_t m_mouseY;
                int32_t m_mouseButton;
        };
    }
}
#endif // HESPERIA_THREED_GLUTCAMERACONTROLLER_H_

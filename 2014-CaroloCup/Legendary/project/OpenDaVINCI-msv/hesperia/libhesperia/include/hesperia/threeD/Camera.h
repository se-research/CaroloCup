/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_THREED_CAMERA_H_
#define HESPERIA_THREED_CAMERA_H_

#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace threeD {

    class Camera
    {
            /**
             * Simple Camera that can be positioned in 3D space with orientation
             * given by head, pitch and roll angles.
             */
            public:
                /**
                 * Creates a new camera located in the origin (0,0,0) with
                 * orientation head,pitch, roll = 0.
                 */
                Camera();

                /**
                 * Creates a new camera with given position and orientation
                 */
                Camera(const core::data::environment::Point3& position,
                       const float& head,
                       const float& pitch,
                       const float& roll);

                ~Camera();

                /**
                 * Moves the camera backwards along the view direction.
                 * position += length*orientation
                 */
                void moveBackward(const float& length);

                /**
                 * Moves the camera forward along the view direction.
                 * position -= length*orientation
                 */
                void moveFoward(const float& length);

                /**
                 * Move the camera up
                 * position.z -= length*orientation.z
                 */
                void moveUp(const float& length);

                /**
                 * Move the camera down
                 * position.z += length*orientation.z
                 */
                void moveDown(const float& length);

                /**
                 * Moves the camera right (based on 2D normal vector of
                 * the orientation).
                 * normal = (orientation.y, -orientation.x, 0)
                 * position += length*normal;
                 */
                void strafeRight(const float& length);

                /**
                 * Moves the camera left (based on 2D normal vector of
                 * the orientation).
                 * normal = (orientation.y, -orientation.x, 0)
                 * position -= length*normal;
                 */
                void strafeLeft(const float& length);

                /**
                 * Changes the head angle by delta degrees.
                 */
                void changeHead(const float& delta);

                /**
                 * Changes the pitch angle by delta degrees.
                 */
                void changePitch(const float& delta);

                /**
                 * Changes the roll angle by delta degrees.
                 */
                void changeRoll(const float& delta);


                /**
                 * Sets a new camera position.
                 */
                void setPosition(const core::data::environment::Point3& position);

                /**
                 * Sets new head angle.
                 * @param head New head angle.
                 */
                void setHead(const float& head);

                /**
                 * Sets new pitch angle.
                 * @param pitch New pitch angle.
                 */
                void setPitch(const float& pitch);

                /**
                 * Sets new roll angle.
                 * @param roll New roll angle.
                 */
                void setRoll(const float& roll);

                /**
                 * Applies this camera in OpenGL
                 */
                void apply();

            private:
                core::data::environment::Point3 m_position;
                core::data::environment::Point3 m_orientation;
                core::data::environment::Point3 m_up;

                float m_head;
                float m_pitch;
                float m_roll;
        };
    }
}
#endif // HESPERIA_THREED_CAMERA_H_

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/primitives/Point.h"

namespace hesperia {
    namespace scenegraph {
        namespace primitives {

            using namespace core::data::environment;

            Point::Point(const SceneNodeDescriptor &sceneNodeDescriptor, const Point3 &position, const Point3 &color, const float &width) :
                SceneNode(sceneNodeDescriptor),
                m_position(position),
                m_color(color),
                m_width(width) {}

            Point::Point(const Point &obj) :
                SceneNode(obj.getSceneNodeDescriptor()),
                m_position(obj.m_position),
                m_color(obj.m_color),
                m_width(obj.m_width) {}

            Point::~Point() {}

            Point& Point::operator=(const Point &obj) {
                setSceneNodeDescriptor(obj.getSceneNodeDescriptor());
                m_position = obj.m_position;
                m_color = obj.m_color;
                m_width = obj.m_width;

                return (*this);
            }

            const Point3& Point::getPosition() const {
                return m_position;
            }

            const Point3& Point::getColor() const {
                return m_color;
            }

            float Point::getWidth() const {
                return m_width;
            }

        }
    }
} // hesperia::scenegraph::primitives

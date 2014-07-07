/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_PRIMITIVES_POINT_H_
#define HESPERIA_SCENEGRAPH_PRIMITIVES_POINT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/scenegraph/SceneNode.h"
#include "hesperia/scenegraph/SceneNodeDescriptor.h"

namespace hesperia {
    namespace scenegraph {
        namespace primitives {

            using namespace std;

            /**
             * This class represents a regular point.
             */
            class Point : public SceneNode {
                public:
                    /**
                     * Constructor.
                     *
                     * @param sceneNodeDesciptor Description for this scene node.
                     * @param position Point's position.
                     * @param color Point's color.
                     * @param width Point's width.
                     */
                    Point(const SceneNodeDescriptor &sceneNodeDescriptor, const core::data::environment::Point3 &position, const core::data::environment::Point3 &color, const float &width);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Point(const Point &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Point& operator=(const Point &obj);

                    virtual ~Point();

                    /**
                     * @return Position.
                     */
                    const core::data::environment::Point3& getPosition() const;

                    /**
                     * @return Point's color.
                     */
                    const core::data::environment::Point3& getColor() const;

                    /**
                     * @return Point's width.
                     */
                    float getWidth() const;

                private:
                    core::data::environment::Point3 m_position;
                    core::data::environment::Point3 m_color;
                    float m_width;
            };

        }
    }
} // hesperia::scenegraph::primitives

#endif /*HESPERIA_SCENEGRAPH_PRIMITIVES_POINT_H_*/

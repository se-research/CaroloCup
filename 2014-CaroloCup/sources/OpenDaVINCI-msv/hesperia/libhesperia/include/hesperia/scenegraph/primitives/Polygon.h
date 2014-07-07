/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_PRIMITIVES_POLYGON_H_
#define HESPERIA_SCENEGRAPH_PRIMITIVES_POLYGON_H_

#include <vector>

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
             * This class represents a polygon.
             */
            class OPENDAVINCI_API Polygon : public SceneNode {
                public:
                    /**
                     * Constructor.
                     *
                     * @param sceneNodeDesciptor Description for this node.
                     * @param listOfGroundVertices List of vertices describing the shape of this polygon.
                     * @param color Color of this polygon.
                     * @param height Polygon's height.
                     */
                    Polygon(const SceneNodeDescriptor &sceneNodeDescriptor, const vector<core::data::environment::Point3> &listOfGroundVertices, const core::data::environment::Point3 &color, const float &height);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Polygon(const Polygon &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Polygon& operator=(const Polygon &obj);

                    virtual ~Polygon();

                    /**
                     * @return List of ground vertices.
                     */
                    const vector<core::data::environment::Point3>& getListOfGroundVertices() const;

                    /**
                     * @return Polygon's color.
                     */
                    const core::data::environment::Point3& getColor() const;

                    /**
                     * @return Polygon's height.
                     */
                    float getHeight() const;

                private:
                    vector<core::data::environment::Point3> m_listOfGroundVertices;
                    core::data::environment::Point3 m_color;
                    float m_height;
            };

        }
    }
} // hesperia::scenegraph::primitives

#endif /*HESPERIA_SCENEGRAPH_PRIMITIVES_POLYGON_H_*/

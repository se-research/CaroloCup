/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_POLYGON_H_
#define HESPERIA_CORE_THREED_MODELS_POLYGON_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace std;

            /**
             * This class represents a polygon.
             */
            class OPENDAVINCI_API Polygon : public Node {
                public:
                    /**
                     * Constructor.
                     *
                    * @param nodeDesciptor Description for this node.
                     * @param listOfGroundVertices List of vertices describing the shape of this polygon.
                     * @param color Color of this polygon.
                     * @param height Polygon's height.
                     */
                    Polygon(const NodeDescriptor &nodeDescriptor, const vector<core::data::environment::Point3> &listOfGroundVertices, const core::data::environment::Point3 &color, const float &height);

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

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    vector<core::data::environment::Point3> m_listOfGroundVertices;
                    core::data::environment::Point3 m_color;
                    float m_height;
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_POLYGON_H_*/

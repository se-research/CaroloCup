/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_TRIANGLESET_H_
#define HESPERIA_CORE_THREED_MODELS_TRIANGLESET_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/threeD/Material.h"
#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/models/Triangle.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace std;

            /**
             * This class represents a set of triangles.
             */
            class OPENDAVINCI_API TriangleSet : public Node {
                public:
                    TriangleSet();

                    /**
                     * Constructor.
                     *
                     * @param nodeDesciptor Description for this node.
                     */
                    TriangleSet(const NodeDescriptor &nodeDescriptor);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    TriangleSet(const TriangleSet &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    TriangleSet& operator=(const TriangleSet &obj);

                    virtual ~TriangleSet();

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                    /**
                     * This method adds a new triangle.
                     *
                     * @param triangle Triangle to be added.
                     */
                    void addTriangle(const Triangle &triangle);

                    /**
                     * This method sets the material for this triangle set.
                     *
                     * @param material Material for this triangle set.
                     */
                    void setMaterial(const Material &material);

                private:
                    mutable bool m_compiled;
                    mutable uint32_t m_callList;
                    Material m_material;
                    vector<core::data::environment::Point3> m_vertices;
                    vector<core::data::environment::Point3> m_normals;
                    vector<core::data::environment::Point3> m_textureCoordinates;

                    /**
                     * This method compiles this triangle set using OpenGL
                     * compile lists.
                     */
                    void compile() const;
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_TRIANGLESET_H_*/

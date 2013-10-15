/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DECORATOR_MODELS_TRIANGLESET_H_
#define HESPERIA_CORE_DECORATOR_MODELS_TRIANGLESET_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/decorator/models/Material.h"
#include "hesperia/decorator/models/Triangle.h"

namespace hesperia {
    namespace decorator {
        namespace models {

            using namespace std;

            /**
             * This class represents a set of triangles.
             */
            class OPENDAVINCI_API TriangleSet {
                public:
                    TriangleSet();

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

                    Material m_material;
                    vector<core::data::environment::Point3> m_vertices;
                    vector<core::data::environment::Point3> m_normals;
                    vector<core::data::environment::Point3> m_textureCoordinates;
            };

        }
    }
} // hesperia::decorator::models

#endif /*HESPERIA_CORE_DECORATOR_MODELS_TRIANGLESET_H_*/

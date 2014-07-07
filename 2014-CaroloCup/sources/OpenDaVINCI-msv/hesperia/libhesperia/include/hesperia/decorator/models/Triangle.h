/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DECORATOR_MODELS_TRIANGLE_H_
#define HESPERIA_CORE_DECORATOR_MODELS_TRIANGLE_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace decorator {
        namespace models {

            using namespace std;

            /**
             * This class represents a triangle.
             */
            class OPENDAVINCI_API Triangle {
                public:
                    Triangle();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Triangle(const Triangle &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Triangle& operator=(const Triangle &obj);

                    virtual ~Triangle();

                    /**
                     * This method sets the triangle's vertices.
                     *
                     * @param a Vertex a.
                     * @param b Vertex b.
                     * @param c Vertex c.
                     */
                    void setVertices(const core::data::environment::Point3 &a, const core::data::environment::Point3 &b, const core::data::environment::Point3 &c);

                    /**
                     * This method returns the triangle's vertices.
                     *
                     * @return Triangle's vertices.
                     */
                    vector<core::data::environment::Point3> getVertices() const;

                    /**
                     * This method sets the triangle's normal.
                     *
                     * @param n Normal.
                     */
                    void setNormal(const core::data::environment::Point3 &n);

                    /**
                     * This method returns the triangle's normal.
                     *
                     * @return Triangle's normal.
                     */
                    core::data::environment::Point3 getNormal() const;

                    /**
                     * This method sets the triangle's texture coordinates.
                     *
                     * @param ta Texture coordinate for vertex a.
                     * @param tc Texture coordinate for vertex b.
                     * @param tb Texture coordinate for vertex c.
                     */
                    void setTextureCoordinates(const core::data::environment::Point3 &ta, const core::data::environment::Point3 &tb, const core::data::environment::Point3 &tc);

                    /**
                     * This method returns the triangle's texture coordinates.
                     *
                     * @return Triangle's texture coordinates.
                     */
                    vector<core::data::environment::Point3> getTextureCoordinates() const;

                private:
                    vector<core::data::environment::Point3> m_vertices;
                    core::data::environment::Point3 m_normal;
                    vector<core::data::environment::Point3> m_textureCoordinates;
            };

        }
    }
} // hesperia::decorator::models

#endif /*HESPERIA_CORE_DECORATOR_MODELS_TRIANGLE_H_*/

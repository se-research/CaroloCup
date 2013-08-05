/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_ENVIRONMENT_POLYGON_H_
#define HESPERIA_DATA_ENVIRONMENT_POLYGON_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;

            using namespace core::data::environment;

            /**
             * This class represents a polygon.
             */
            class OPENDAVINCI_API Polygon : public core::data::SerializableData {
                private:
                    const static double EPSILON;

                public:
                    Polygon();

                    /**
                     * Constructor.
                     *
                     * @param List of vertices.
                     */
                    Polygon(const vector<Point3> &vertices);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Polygon(const Polygon &obj);

                    virtual ~Polygon();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Polygon& operator=(const Polygon &obj);

                    /**
                     * This method adds a point to a polygon.
                     *
                     * @param p Point to be added.
                     */
                    void add(const Point3 &p);

                    /**
                     * This method returns all vertices.
                     *
                     * @return Vertices from this polygon.
                     */
                    vector<Point3> getVertices() const;

                    /**
                     * This method returns this polygon's size.
                     *
                     * @return Number of vertices.
                     */
                    uint32_t getSize() const;

                    /**
                     * This method checks if the given point
                     * is within this polygon ignoring Z coordinate.
                     *
                     * @param p Point to be tested.
                     * @return true, if p is within this polygon.
                     */
                    bool containsIgnoreZ(const Point3 &p) const;

                    /**
                     * This method intersects this polygon with
                     * the other return the resulting contour
                     * ignoring the Z coordinate.
                     *
                     * @param other Polygon to be intersect with this one.
                     * @return Polygon built from intersection points.
                     */
                    Polygon intersectIgnoreZ(const Polygon &other) const;

                    /**
                     * This method returns only the visible vertices
                     * for a given point ignoring the Z coordinate.
                     *
                     * @param point Point for visibility computation.
                     * @return Polygon containing only directly visible vertices.
                     */
                    Polygon getVisiblePolygonIgnoreZ(const Point3 &point) const;

                    /**
                     * This method computes the center of the polygon.
                     *
                     * @return Center of the polyon.
                     */
                    Point3 getCenter() const;

                    /**
                     * This method sorts all contained vertices to
                     * avoid crossing lines.
                     */
                    void sort();

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    vector<Point3> m_listOfVertices;
            };

        }
    }
} // hesperia::data::environment

#endif /*HESPERIA_DATA_ENVIRONMENT_POINT3_H_*/

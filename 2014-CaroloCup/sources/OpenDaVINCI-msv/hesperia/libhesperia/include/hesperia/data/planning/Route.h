/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_PLANNING_ROUTE_H_
#define HESPERIA_DATA_PLANNING_ROUTE_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace data {
        namespace planning {

            using namespace std;

            /**
             * This class represents a route consisting of several points.
             */
            class OPENDAVINCI_API Route : public core::data::SerializableData {
                public:
                    Route();

                    /**
                     * Constructor.
                     *
                     * @param List of vertices.
                     */
                    Route(const vector<core::data::environment::Point3> &vertices);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Route(const Route &obj);

                    virtual ~Route();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Route& operator=(const Route &obj);

                    /**
                     * This method adds a point to a polygon.
                     *
                     * @param p Point to be added.
                     */
                    void add(const core::data::environment::Point3 &p);

                    /**
                     * This method returns all vertices.
                     *
                     * @return Vertices from this polygon.
                     */
                    vector<core::data::environment::Point3> getListOfPoints() const;

                    /**
                     * This method returns this route size.
                     *
                     * @return Number of vertices.
                     */
                    uint32_t getSize() const;

                    /**
                     * This method computes the length of the route.
                     *
                     * @return Length of the route.
                     */
                    double getLength() const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    vector<core::data::environment::Point3> m_listOfVertices;
            };

        }
    }
} // hesperia::data::planning

#endif /*HESPERIA_DATA_PLANNING_ROUTE_H_*/

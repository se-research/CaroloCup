/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_GRAPH_WAYPOINTSEDGE_H_
#define HESPERIA_DATA_GRAPH_WAYPOINTSEDGE_H_

#include <stdint.h>

#include "core/wrapper/graph/Edge.h"

namespace hesperia {
    namespace data {
        namespace graph{

            using namespace std;

            /**
             * This class implements the core::wrapper::Edge interface for implementing
             * a digital map using the road network from the scenario.
             */
            class WaypointsEdge : public core::wrapper::graph::Edge {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    WaypointsEdge(const WaypointsEdge &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    WaypointsEdge& operator=(const WaypointsEdge &/*obj*/);

                public:
                    WaypointsEdge();

                    virtual ~WaypointsEdge();

                    /**
                     * This method sets the costs.
                     *
                     * @param c Costs.
                     */
                    void setCosts(const double &c);

                    virtual const string toString() const;

                    virtual double getCosts() const;

                private:
                    double m_costs;
            };

        }
    }
} // hesperia::data::graph

#endif /*HESPERIA_DATA_GRAPH_WAYPOINTSEDGE_H_*/

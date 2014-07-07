/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_WRAPPER_GRAPH_EDGE_H_
#define HESPERIA_WRAPPER_GRAPH_EDGE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {
        namespace graph {

            using namespace std;

            /**
             * This interface encapsulates all necessary method for a graph's edge.
             */
            class OPENDAVINCI_API Edge {
                public:
                    virtual ~Edge();

                    /**
                     * This method returns a human readable representation.
                     *
                     * @return Human readable representation.
                     */
                    virtual const string toString() const = 0;

                    /**
                     * This method returns the costs between both vertices.
                     *
                     * @return Costs.
                     */
                    virtual double getCosts() const = 0;
            };

        }
    }
} // core::wrapper::graph

#endif /*HESPERIA_WRAPPER_GRAPH_EDGE_H_*/

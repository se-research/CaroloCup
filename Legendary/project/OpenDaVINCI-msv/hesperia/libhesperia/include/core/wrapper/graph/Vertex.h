/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_WRAPPER_GRAPH_VERTEX_H_
#define HESPERIA_WRAPPER_GRAPH_VERTEX_H_

#include <stdint.h>

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {
        namespace graph {

            using namespace std;

            /**
             * This interface encapsulates all necessary method for a graph's vertex.
             */
            class OPENDAVINCI_API Vertex {
                public:
                    virtual ~Vertex();

                    /**
                     * This method returns a human readable representation.
                     *
                     * @return Human readable representation.
                     */
                    virtual const string toString() const = 0;

                    /**
                     * This method returns a unique identifier for this vertex.
                     *
                     * @return Unique identifier.
                     */
                    virtual int32_t getIdentifier() const = 0;

                    /**
                     * This method returns the distance to the
                     * given other vertex.
                     *
                     * @param v2 Other vertex.
                     * @return Distance to v2 or -1 if v2 is invalid.
                     */
                    virtual double getDistanceTo(const Vertex &v2) const = 0;
            };

        }
    }
} // core::wrapper::graph

#endif /*HESPERIA_WRAPPER_GRAPH_VERTEX_H_*/

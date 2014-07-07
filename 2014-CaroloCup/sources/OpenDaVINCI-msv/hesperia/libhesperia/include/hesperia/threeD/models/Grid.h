/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_GRID_H_
#define HESPERIA_CORE_THREED_MODELS_GRID_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            /**
             * This class represents a grid in the XY-layer.
             */
            class OPENDAVINCI_API Grid : public Node {
                public:
                    /**
                     * Constructor.
                     *
                    * @param nodeDesciptor Description for this node.
                     * @param size Size of the grid in positive XY-direction (i.e. the resulting grid is 2*size).
                     * @param lineWidth Width of the grid's lines.
                     */
                    Grid(const NodeDescriptor &nodeDescriptor, const uint32_t &size, const float &lineWidth);

                    virtual ~Grid();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Grid(const Grid &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Grid& operator=(const Grid &obj);

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    uint32_t m_size;
                    float m_lineWidth;
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_GRID_H_*/

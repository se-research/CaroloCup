/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_MODELS_GRID_H_
#define HESPERIA_SCENEGRAPH_MODELS_GRID_H_

#include <vector>

#include "core/platform.h"

#include "core/data/environment/Point3.h"
#include "hesperia/scenegraph/SceneNode.h"
#include "hesperia/scenegraph/SceneNodeDescriptor.h"
#include "hesperia/scenegraph/primitives/Line.h"

namespace hesperia {
    namespace scenegraph {
        namespace models {

            using namespace std;

            /**
             * This class represents grid.
             */
            class OPENDAVINCI_API Grid : public SceneNode {
                public:
                    /**
                     * Constructor.
                     *
                     * @param sceneNodeDesciptor Description for this node.
                     */
                    Grid(const SceneNodeDescriptor &sceneNodeDescriptor);

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

                    virtual ~Grid();

                private:
                    void createGrid();
            };

        }
    }
} // hesperia::scenegraph::models

#endif /*HESPERIA_SCENEGRAPH_MODELS_GRID_H_*/

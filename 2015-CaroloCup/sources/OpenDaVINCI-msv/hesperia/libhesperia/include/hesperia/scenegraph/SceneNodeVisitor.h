/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_SCENENODEVISITOR_H_
#define HESPERIA_SCENEGRAPH_SCENENODEVISITOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace scenegraph {

        // Forward declaration for SceneNode.
        class SceneNode;

        /**
         * Interface for all SceneNodeVisitors.
         */
        class OPENDAVINCI_API SceneNodeVisitor {
            public:
                virtual ~SceneNodeVisitor();

                /**
                 * This method is called from a visited scene node.
                 *
                 * @param sceneNode Scene node to visit.
                 */
                virtual void visit(SceneNode *sceneNode) = 0;
        };

    }
} // hesperia::scenegraph

#endif /*HESPERIA_SCENEGRAPH_SCENENODEVISITOR_H_*/

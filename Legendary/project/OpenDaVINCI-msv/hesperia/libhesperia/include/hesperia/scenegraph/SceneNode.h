/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_SCENENODE_H_
#define HESPERIA_SCENEGRAPH_SCENENODE_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Mutex.h"

#include "hesperia/scenegraph/SceneNodeDescriptor.h"
#include "hesperia/scenegraph/SceneNodeVisitor.h"

namespace hesperia {
    namespace scenegraph {

        /**
         * An element of the scenegraph. On destruction, all children will be destroyed!
         */
        class OPENDAVINCI_API SceneNode {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SceneNode(const SceneNode &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SceneNode& operator=(const SceneNode &);

            public:
                SceneNode();

                /**
                 * Constructor.
                 *
                 * @param sceneNodeDescriptor Description of this node.
                 */
                SceneNode(const SceneNodeDescriptor &sceneNodeDescriptor);

                virtual ~SceneNode();

                /**
                 * This method returns this scene node's description.
                 *
                 * @return This scene node's descriptor.
                 */
                const SceneNodeDescriptor getSceneNodeDescriptor() const;

                /**
                 * This method sets the scene node descriptor.
                 *
                 * @param sceneNodeDescriptor Scene node description to be set.
                 */
                void setSceneNodeDescriptor(const SceneNodeDescriptor &sceneNodeDescriptor);

                /**
                 * This method adds a scene node as child.
                 *
                 * @param sceneNode Scene node to be added.
                 */
                void addChild(SceneNode *sceneNode);

                /**
                 * This method removes a child.
                 *
                 * @param c Child to be removed.
                 */
                void removeChild(SceneNode *c);

                /**
                 * This method deletes all registered children.
                 */
                void deleteAllChildren();

                /**
                 * @return the number of children.
                 */
                uint32_t getNumberOfChildren();

                /**
                 * This method accepts a visitor.
                 *
                 * @param sceneNodeVisitor Scene node to be added.
                 */
                void accept(SceneNodeVisitor &sceneNodeVisitor);

            private:
                SceneNodeDescriptor m_sceneNodeDescriptor;

                core::base::Mutex m_childrenMutex;
                vector<SceneNode*> m_children;
        };

    }
} // hesperia::scenegraph

#endif /*HESPERIA_SCENEGRAPH_SCENENODE_H_*/

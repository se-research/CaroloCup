/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "hesperia/scenegraph/SceneNode.h"

namespace hesperia {
    namespace scenegraph {

        using namespace core::base;

        SceneNode::SceneNode() :
            m_sceneNodeDescriptor(),
            m_childrenMutex(),
            m_children() {}

        SceneNode::SceneNode(const SceneNodeDescriptor &sceneNodeDescriptor) :
            m_sceneNodeDescriptor(sceneNodeDescriptor),
            m_childrenMutex(),
            m_children() {}

        SceneNode::~SceneNode() {
            deleteAllChildren();
        }

        const SceneNodeDescriptor SceneNode::getSceneNodeDescriptor() const {
            return m_sceneNodeDescriptor;
        }

        void SceneNode::setSceneNodeDescriptor(const SceneNodeDescriptor &sceneNodeDescriptor) {
            m_sceneNodeDescriptor = sceneNodeDescriptor;
        }

        void SceneNode::addChild(SceneNode *sceneNode) {
            if (sceneNode != NULL) {
                Lock l(m_childrenMutex);
                m_children.push_back(sceneNode);
            }
        }

        void SceneNode::removeChild(SceneNode *c) {
            Lock l(m_childrenMutex);

            if (c != NULL) {
                vector<SceneNode*>::iterator result = find(m_children.begin(), m_children.end(), c);
                if (result != m_children.end()) {
                    SceneNode *sn = *result;
                    sn->deleteAllChildren();

                    OPENDAVINCI_CORE_DELETE_POINTER(sn);

                    m_children.erase(result);
                }
            }
        }

        void SceneNode::deleteAllChildren() {
            Lock l(m_childrenMutex);

            vector<SceneNode*>::iterator it = m_children.begin();
            while (it != m_children.end()) {
                SceneNode *sn = (*it++);

                sn->deleteAllChildren();

                OPENDAVINCI_CORE_DELETE_POINTER(sn);
            }
            m_children.clear();
        }

        uint32_t SceneNode::getNumberOfChildren() {
            Lock l(m_childrenMutex);
            return m_children.size();
        }

        void SceneNode::accept(SceneNodeVisitor &sceneNodeVisitor) {
            Lock l(m_childrenMutex);

            sceneNodeVisitor.visit(this);

            vector<SceneNode*>::iterator it = m_children.begin();
            while (it != m_children.end()) {
                SceneNode *sn = (*it++);
                sn->accept(sceneNodeVisitor);
            }
        }

    }
} // hesperia::scenegraph

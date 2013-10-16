/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/SceneNodeDescriptor.h"

#include "plugins/birdseyemap/TreeNodeVisitor.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            using namespace std;
            using namespace core::base;
            using namespace hesperia::scenegraph;
            using namespace hesperia::scenegraph::renderer;

            TreeNodeVisitor::TreeNodeVisitor(RenderingConfiguration &rc, TreeNode<SelectableNodeDescriptor> *root) :
                m_renderingConfiguration(rc),
                m_root(root) {}

            TreeNodeVisitor::~TreeNodeVisitor() {}

            void TreeNodeVisitor::visit(SceneNode *n) {
                if (m_root != NULL) {
                    if (n != NULL) {
                        SceneNodeDescriptor snd = n->getSceneNodeDescriptor();
                        if (snd.getName().size() > 0) {
                            if (!m_renderingConfiguration.hasSceneNodeDescriptor(snd)) {
                                // Enable the node described by this NodeDescriptor for rendering by default.
                                SceneNodeRenderingConfiguration snrc;
                                snrc.setParameter(SceneNodeRenderingConfiguration::ENABLED, true);
                                m_renderingConfiguration.setSceneNodeRenderingConfiguration(snd, snrc);

                                // Add NodeDescriptor to selectable list.
                                TreeNode<SelectableNodeDescriptor> *element = new TreeNode<SelectableNodeDescriptor>();
                                element->setValue(SelectableNodeDescriptor(snd, snrc.hasParameter(SceneNodeRenderingConfiguration::ENABLED)));
                                m_root->addChild(element);
                            }
                        }
                    }
                }
            }
        }
    }
} // plugins::birdseyemap

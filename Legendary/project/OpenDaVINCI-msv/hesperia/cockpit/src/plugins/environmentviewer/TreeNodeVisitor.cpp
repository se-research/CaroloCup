/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/NodeRenderingConfiguration.h"

#include "plugins/environmentviewer/TreeNodeVisitor.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

            using namespace std;
            using namespace core::base;
            using namespace hesperia::threeD;

            TreeNodeVisitor::TreeNodeVisitor(RenderingConfiguration &rc, TreeNode<SelectableNodeDescriptor> *root) :
                m_renderingConfiguration(rc),
                m_root(root) {}

            TreeNodeVisitor::~TreeNodeVisitor() {}

            void TreeNodeVisitor::visit(Node *n) {
                if (m_root != NULL) {
                    if (n != NULL) {
                        NodeDescriptor nd = n->getNodeDescriptor();
                        if (nd.getName().size() > 0) {
                            // Enable the node described by this NodeDescriptor for rendering by default.
                            NodeRenderingConfiguration nrc;
                            nrc.setParameter(NodeRenderingConfiguration::ENABLED, true);
                            m_renderingConfiguration.setNodeRenderingConfiguration(nd, nrc);

                            // Add NodeDescriptor to selectable list.
                            TreeNode<SelectableNodeDescriptor> *element = new TreeNode<SelectableNodeDescriptor>();
                            element->setValue(SelectableNodeDescriptor(nd, nrc.hasParameter(NodeRenderingConfiguration::ENABLED)));
                            m_root->addChild(element);
                        }
                    }
                }
            }
        }
    }
} // plugins::environmentviewer

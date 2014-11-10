/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLUGINS_BIRDSEYEMAP_TREENODEVISITOR_H_
#define PLUGINS_BIRDSEYEMAP_TREENODEVISITOR_H_

#include "core/base/TreeNode.h"
#include "hesperia/scenegraph/SceneNode.h"
#include "hesperia/scenegraph/SceneNodeVisitor.h"
#include "hesperia/scenegraph/renderer/RenderingConfiguration.h"

#include "plugins/birdseyemap/SelectableNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            using namespace std;

            /**
             * This class represents SceneNodeDescriptor combined with a state
             * indicating whether this element was selected by the user.
             */
            class TreeNodeVisitor : public hesperia::scenegraph::SceneNodeVisitor {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    TreeNodeVisitor(const TreeNodeVisitor &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    TreeNodeVisitor& operator=(const TreeNodeVisitor &);

                public:
                    /**
                     * Constructor.
                     *
                     * @param root TreeNode for adding SceneNodeDescriptors.
                     */
                    TreeNodeVisitor(hesperia::scenegraph::renderer::RenderingConfiguration &rc, core::base::TreeNode<SelectableNodeDescriptor> *root);

                    virtual ~TreeNodeVisitor();

                    virtual void visit(hesperia::scenegraph::SceneNode *snd);

                private:
                    hesperia::scenegraph::renderer::RenderingConfiguration &m_renderingConfiguration;
                    core::base::TreeNode<SelectableNodeDescriptor> *m_root;
            };
        }
    }
} // plugins::birdseyemap

#endif /*PLUGINS_BIRDSEYEMAP_TREENODEVISITOR_H_*/

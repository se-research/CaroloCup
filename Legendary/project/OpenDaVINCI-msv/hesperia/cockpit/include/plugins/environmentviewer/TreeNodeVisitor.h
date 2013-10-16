/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_ENVIRONMENTVIEWER_TREENODEVISITOR_H_
#define PLUGINS_ENVIRONMENTVIEWER_TREENODEVISITOR_H_

#include "core/base/TreeNode.h"

#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/RenderingConfiguration.h"
#include "hesperia/threeD/TransformGroupVisitor.h"

#include "plugins/environmentviewer/SelectableNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

            using namespace std;

            /**
             * This class represents NodeDescriptor combined with a state
             * indicating whether this element was selected by the user.
             */
            class TreeNodeVisitor : public hesperia::threeD::TransformGroupVisitor {
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
                     * @param rc RenderingConfiguration.
                     * @param root TreeNode for adding NodeDescriptors.
                     */
                    TreeNodeVisitor(hesperia::threeD::RenderingConfiguration &rc, core::base::TreeNode<SelectableNodeDescriptor> *root);

                    virtual ~TreeNodeVisitor();

                    virtual void visit(hesperia::threeD::Node *nd);

                private:
                    hesperia::threeD::RenderingConfiguration &m_renderingConfiguration;
                    core::base::TreeNode<SelectableNodeDescriptor> *m_root;
            };
        }
    }
} // plugins::environmentviewer

#endif /*PLUGINS_ENVIRONMENTVIEWER_TREENODEVISITOR_H_*/

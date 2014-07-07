/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_ENVIRONMENTVIEWER_SELECTABLENODEDESCRIPTORTREELISTENER_H_
#define PLUGINS_ENVIRONMENTVIEWER_SELECTABLENODEDESCRIPTORTREELISTENER_H_

#include "core/base/TreeNode.h"

#include "plugins/environmentviewer/SelectableNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

            /**
             * This interface encapsulates the access to the tree of SelectableNodeDescriptors.
             */
            class SelectableNodeDescriptorTreeListener {
                public:
                    virtual ~SelectableNodeDescriptorTreeListener();

                    /**
                     * This method is called by to update the SelectableNodeDescriptor.
                     *
                     * @param node Node of the tree to update.
                     */
                    virtual void update(core::base::TreeNode<SelectableNodeDescriptor> *node) = 0;
            };
        }
    }
} // plugins::environmentviewer

#endif /*PLUGINS_ENVIRONMENTVIEWER_SELECTABLENODEDESCRIPTORTREELISTENER_H_*/

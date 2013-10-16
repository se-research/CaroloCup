/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLUGINS_BIRDSEYEMAP_SELECTABLENODEDESCRIPTORTREELISTENER_H_
#define PLUGINS_BIRDSEYEMAP_SELECTABLENODEDESCRIPTORTREELISTENER_H_

#include "core/base/TreeNode.h"

#include "plugins/birdseyemap/SelectableNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

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
} // plugins::birdseyemap

#endif /*PLUGINS_BIRDSEYEMAP_SELECTABLENODEDESCRIPTORTREELISTENER_H_*/

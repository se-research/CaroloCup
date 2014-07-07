/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLUGINS_BIRDSEYEMAP_CAMERAASSIGNABLENODESLISTENER_H_
#define PLUGINS_BIRDSEYEMAP_CAMERAASSIGNABLENODESLISTENER_H_

#include <vector>

#include "hesperia/scenegraph/SceneNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            using namespace std;

            /**
             * This interface encapsulates the access for getting a list
             * of node to which a camera can be assigned.
             */
            class CameraAssignableNodesListener {
                public:
                    virtual ~CameraAssignableNodesListener();

                    /**
                     * This method is called by EnvironmentViewerGLWidget to update
                     * the list of camera assignable nodes.
                     *
                     * @param list List of camera assignable nodes.
                     */
                    virtual void updateListOfCameraAssignableNodes(const vector<hesperia::scenegraph::SceneNodeDescriptor> &list) = 0;
            };

        }
    }
} // plugins::birdseyemap

#endif /*PLUGINS_BIRDSEYEMAP_CAMERAASSIGNABLENODESLISTENER_H_*/

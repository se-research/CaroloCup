/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_ENVIRONMENTVIEWER_CAMERAASSIGNABLENODESLISTENER_H_
#define PLUGINS_ENVIRONMENTVIEWER_CAMERAASSIGNABLENODESLISTENER_H_

#include <vector>

#include "hesperia/threeD/NodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

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
                    virtual void updateListOfCameraAssignableNodes(const vector<hesperia::threeD::NodeDescriptor> &list) = 0;
            };

        }
    }
} // plugins::environmentviewer

#endif /*PLUGINS_ENVIRONMENTVIEWER_CAMERAASSIGNABLENODESLISTENER_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLUGINS_BIRDSEYEMAP_SELECTABLENODESCRIPTOR_H_
#define PLUGINS_BIRDSEYEMAP_SELECTABLENODESCRIPTOR_H_

#include "hesperia/scenegraph/SceneNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            using namespace std;

            /**
             * This class represents NodeDescriptor combined with a state
             * indicating whether this element was selected by the user.
             */
            class SelectableNodeDescriptor {
                public:
                    SelectableNodeDescriptor();

                    /**
                     * Constructor.
                     *
                     * @param snd SceneNodeDescriptor.
                     * @param selected True iff selected.
                     */
                    SelectableNodeDescriptor(const hesperia::scenegraph::SceneNodeDescriptor &snd, const bool &selected);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    SelectableNodeDescriptor(const SelectableNodeDescriptor &obj);

                    virtual ~SelectableNodeDescriptor();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    SelectableNodeDescriptor& operator=(const SelectableNodeDescriptor &obj);

                    /**
                     * This method returns the SceneNodeDescriptor.
                     *
                     * @return Name.
                     */
                    const hesperia::scenegraph::SceneNodeDescriptor getSceneNodeDescriptor() const;

                    /**
                     * This method sets the SceneNodeDescriptor.
                     *
                     * @param snd SceneNodeDescriptor.
                     */
                    void setSceneNodeDescriptor(const hesperia::scenegraph::SceneNodeDescriptor &snd);

                    /**
                     * This method returns true if this SceneNodeDescriptor is selected.
                     *
                     * @return True iff this SceneNodeDescriptor element is selected.
                     */
                    bool isSelected() const;

                    /**
                     * This method sets the selection.
                     *
                     * @param selected True if this SceneNodeDescriptor is selected.
                     */
                    void setSelected(const bool &selected);

                private:
                    hesperia::scenegraph::SceneNodeDescriptor m_sceneNodeDescriptor;
                    bool m_selected;
            };
        }
    }
} // plugins::birdseyemap

#endif /*PLUGINS_BIRDSEYEMAP_SELECTABLENODESCRIPTOR_H_*/

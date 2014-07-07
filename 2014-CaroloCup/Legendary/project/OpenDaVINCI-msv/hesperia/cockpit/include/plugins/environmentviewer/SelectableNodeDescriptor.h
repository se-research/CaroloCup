/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef PLUGINS_ENVIRONMENTVIEWER_SELECTABLENODESCRIPTOR_H_
#define PLUGINS_ENVIRONMENTVIEWER_SELECTABLENODESCRIPTOR_H_

#include "hesperia/threeD/NodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

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
                     * @param nd NodeDescriptor.
                     * @param selected True iff selected.
                     */
                    SelectableNodeDescriptor(const hesperia::threeD::NodeDescriptor &nd, const bool &selected);

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
                     * This method returns the NodeDescriptor.
                     *
                     * @return Name.
                     */
                    const hesperia::threeD::NodeDescriptor getNodeDescriptor() const;

                    /**
                     * This method sets the NodeDescriptor.
                     *
                     * @param nd NodeDescriptor.
                     */
                    void setNodeDescriptor(const hesperia::threeD::NodeDescriptor &nd);

                    /**
                     * This method returns true if this NodeDescriptor is selected.
                     *
                     * @return True iff this NodeDescriptor element is selected.
                     */
                    bool isSelected() const;

                    /**
                     * This method sets the selection.
                     *
                     * @param selected True if this NodeDescriptor is selected.
                     */
                    void setSelected(const bool &selected);

                private:
                    hesperia::threeD::NodeDescriptor m_nodeDescriptor;
                    bool m_selected;
            };
        }
    }
} // plugins::environmentviewer

#endif /*PLUGINS_ENVIRONMENTVIEWER_SELECTABLENODESCRIPTOR_H_*/

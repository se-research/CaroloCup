/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/environmentviewer/SelectableNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

            using namespace hesperia::threeD;

            SelectableNodeDescriptor::SelectableNodeDescriptor() :
                m_nodeDescriptor(),
                m_selected(false) {}

            SelectableNodeDescriptor::SelectableNodeDescriptor(const NodeDescriptor &nd, const bool &selected) :
                m_nodeDescriptor(nd),
                m_selected(selected) {}

            SelectableNodeDescriptor::SelectableNodeDescriptor(const SelectableNodeDescriptor &obj) :
                m_nodeDescriptor(obj.getNodeDescriptor()),
                m_selected(obj.isSelected()) {}

            SelectableNodeDescriptor::~SelectableNodeDescriptor() {}

            SelectableNodeDescriptor& SelectableNodeDescriptor::operator=(const SelectableNodeDescriptor &obj) {
                m_nodeDescriptor = obj.getNodeDescriptor();
                m_selected = obj.isSelected();

                return (*this);
            }

            const NodeDescriptor SelectableNodeDescriptor::getNodeDescriptor() const {
                return m_nodeDescriptor;
            }

            void SelectableNodeDescriptor::setNodeDescriptor(const NodeDescriptor &nd) {
                m_nodeDescriptor = nd;
            }

            bool SelectableNodeDescriptor::isSelected() const {
                return m_selected;
            }

            void SelectableNodeDescriptor::setSelected(const bool &selected) {
                m_selected = selected;
            }
        }
    }
} // plugins::environmentviewer

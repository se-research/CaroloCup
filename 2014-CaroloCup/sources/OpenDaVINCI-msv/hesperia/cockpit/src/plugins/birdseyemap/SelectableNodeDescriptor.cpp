/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "plugins/birdseyemap/SelectableNodeDescriptor.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            using namespace hesperia::scenegraph;

            SelectableNodeDescriptor::SelectableNodeDescriptor() :
                m_sceneNodeDescriptor(),
                m_selected(false) {}

            SelectableNodeDescriptor::SelectableNodeDescriptor(const SceneNodeDescriptor &snd, const bool &selected) :
                m_sceneNodeDescriptor(snd),
                m_selected(selected) {}

            SelectableNodeDescriptor::SelectableNodeDescriptor(const SelectableNodeDescriptor &obj) :
                m_sceneNodeDescriptor(obj.getSceneNodeDescriptor()),
                m_selected(obj.isSelected()) {}

            SelectableNodeDescriptor::~SelectableNodeDescriptor() {}

            SelectableNodeDescriptor& SelectableNodeDescriptor::operator=(const SelectableNodeDescriptor &obj) {
                m_sceneNodeDescriptor = obj.getSceneNodeDescriptor();
                m_selected = obj.isSelected();

                return (*this);
            }

            const SceneNodeDescriptor SelectableNodeDescriptor::getSceneNodeDescriptor() const {
                return m_sceneNodeDescriptor;
            }

            void SelectableNodeDescriptor::setSceneNodeDescriptor(const SceneNodeDescriptor &snd) {
                m_sceneNodeDescriptor = snd;
            }

            bool SelectableNodeDescriptor::isSelected() const {
                return m_selected;
            }

            void SelectableNodeDescriptor::setSelected(const bool &selected) {
                m_selected = selected;
            }
        }
    }
} // plugins::birdseyemap

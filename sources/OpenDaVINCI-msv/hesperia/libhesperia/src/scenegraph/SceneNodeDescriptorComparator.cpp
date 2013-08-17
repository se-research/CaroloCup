/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/SceneNodeDescriptorComparator.h"

namespace hesperia {
    namespace scenegraph {

        SceneNodeDescriptorComparator::SceneNodeDescriptorComparator() {}

        SceneNodeDescriptorComparator::SceneNodeDescriptorComparator(const SceneNodeDescriptorComparator &/*obj*/) {}

        SceneNodeDescriptorComparator::~SceneNodeDescriptorComparator() {}

        SceneNodeDescriptorComparator& SceneNodeDescriptorComparator::operator=(const SceneNodeDescriptorComparator &/*obj*/) {
            return (*this);
        }

        bool SceneNodeDescriptorComparator::operator()(const SceneNodeDescriptor &snd1, const SceneNodeDescriptor &snd2) const {
            return snd1.getName().compare(snd2.getName()) < 0;
        }

    }
} // hesperia::scenegraph

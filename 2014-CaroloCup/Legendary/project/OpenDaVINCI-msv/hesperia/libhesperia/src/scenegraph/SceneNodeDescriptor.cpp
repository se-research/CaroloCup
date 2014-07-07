/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/SceneNodeDescriptor.h"

namespace hesperia {
    namespace scenegraph {

        using namespace std;

        SceneNodeDescriptor::SceneNodeDescriptor() :
            m_name() {}

        SceneNodeDescriptor::SceneNodeDescriptor(const string &name) :
            m_name(name) {}

        SceneNodeDescriptor::~SceneNodeDescriptor() {}

        SceneNodeDescriptor::SceneNodeDescriptor(const SceneNodeDescriptor &obj) :
            m_name(obj.m_name) {}

        SceneNodeDescriptor& SceneNodeDescriptor::operator=(const SceneNodeDescriptor &obj) {
            m_name = obj.m_name;

            return (*this);
        }

        const string SceneNodeDescriptor::getName() const {
            return m_name;
        }
    }
} // hesperia::scenegraph

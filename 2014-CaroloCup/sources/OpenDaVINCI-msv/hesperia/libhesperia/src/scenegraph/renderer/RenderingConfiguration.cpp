/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/renderer/RenderingConfiguration.h"

namespace hesperia {
    namespace scenegraph {
        namespace renderer {

            RenderingConfiguration::RenderingConfiguration() :
                m_sceneNodesRenderingConfiguration() {}

            RenderingConfiguration::RenderingConfiguration(const RenderingConfiguration &obj) :
            	m_sceneNodesRenderingConfiguration(obj.m_sceneNodesRenderingConfiguration) {}

            RenderingConfiguration::~RenderingConfiguration() {}

            RenderingConfiguration& RenderingConfiguration::operator=(const RenderingConfiguration &obj) {
            	m_sceneNodesRenderingConfiguration = obj.m_sceneNodesRenderingConfiguration;

            	return (*this);
            }

            bool RenderingConfiguration::hasSceneNodeDescriptor(const SceneNodeDescriptor &snd) const {
                map<SceneNodeDescriptor, SceneNodeRenderingConfiguration, SceneNodeDescriptorComparator>::const_iterator it = m_sceneNodesRenderingConfiguration.find(snd);
                return (it != m_sceneNodesRenderingConfiguration.end());
            }

            const SceneNodeRenderingConfiguration& RenderingConfiguration::getSceneNodeRenderingConfiguration(const SceneNodeDescriptor &snd) {
                return m_sceneNodesRenderingConfiguration[snd];
            }

            void RenderingConfiguration::setSceneNodeRenderingConfiguration(const SceneNodeDescriptor &snd, const SceneNodeRenderingConfiguration &snrc) {
                m_sceneNodesRenderingConfiguration[snd] = snrc;
            }

        }
    }
} // hesperia::scenegraph::renderer

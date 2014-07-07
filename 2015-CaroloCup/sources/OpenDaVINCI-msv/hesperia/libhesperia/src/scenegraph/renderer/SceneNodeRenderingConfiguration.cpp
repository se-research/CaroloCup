/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/renderer/SceneNodeRenderingConfiguration.h"

namespace hesperia {
    namespace scenegraph {
        namespace renderer {

            using namespace std;

            SceneNodeRenderingConfiguration::SceneNodeRenderingConfiguration() :
                    m_parameters(SceneNodeRenderingConfiguration::ENABLED) {}

            SceneNodeRenderingConfiguration::~SceneNodeRenderingConfiguration() {}

            SceneNodeRenderingConfiguration::SceneNodeRenderingConfiguration(const SceneNodeRenderingConfiguration &obj) :
                m_parameters(obj.m_parameters) {}

            SceneNodeRenderingConfiguration& SceneNodeRenderingConfiguration::operator=(const SceneNodeRenderingConfiguration &obj) {
                m_parameters = obj.m_parameters;

                return (*this);
            }

            bool SceneNodeRenderingConfiguration::hasParameter(const enum RENDERING_PARAMETERS &p) const {
                return ( (m_parameters & static_cast<uint32_t>(p)) == static_cast<uint32_t>(p));
            }

            void SceneNodeRenderingConfiguration::setParameter(const enum RENDERING_PARAMETERS &p, const bool &enabled) {
                if (enabled) {
                    m_parameters |= p;
                }
                else {
                    m_parameters &= ~p;
                }
            }

        }
    }
} // hesperia::scenegraph::renderer

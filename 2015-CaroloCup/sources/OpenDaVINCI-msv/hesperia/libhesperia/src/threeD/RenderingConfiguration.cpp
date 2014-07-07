/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/threeD/RenderingConfiguration.h"

namespace hesperia {
    namespace threeD {

        RenderingConfiguration::RenderingConfiguration() :
            m_drawTextures(true),
            m_nodesRenderingConfiguration() {}

        RenderingConfiguration::RenderingConfiguration(const RenderingConfiguration &obj) :
        	m_drawTextures(obj.m_drawTextures),
        	m_nodesRenderingConfiguration(obj.m_nodesRenderingConfiguration) {}

        RenderingConfiguration::~RenderingConfiguration() {}

        RenderingConfiguration& RenderingConfiguration::operator=(const RenderingConfiguration &obj) {
        	m_drawTextures = obj.m_drawTextures;
        	m_nodesRenderingConfiguration = obj.m_nodesRenderingConfiguration;

        	return (*this);
        }

        bool RenderingConfiguration::hasDrawTextures() const {
            return m_drawTextures;
        }

        void RenderingConfiguration::setDrawTextures(const bool &drawTextures) {
            m_drawTextures = drawTextures;
        }

        const NodeRenderingConfiguration& RenderingConfiguration::getNodeRenderingConfiguration(const NodeDescriptor &nd) {
            return m_nodesRenderingConfiguration[nd];
        }

        void RenderingConfiguration::setNodeRenderingConfiguration(const NodeDescriptor &nd, const NodeRenderingConfiguration &nrc) {
            m_nodesRenderingConfiguration[nd] = nrc;
        }

    }
} // hesperia::threeD

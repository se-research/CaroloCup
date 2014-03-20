/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/threeD/NodeRenderingConfiguration.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        NodeRenderingConfiguration::NodeRenderingConfiguration() :
                m_parameters(NodeRenderingConfiguration::ENABLED) {}

        NodeRenderingConfiguration::~NodeRenderingConfiguration() {}

        NodeRenderingConfiguration::NodeRenderingConfiguration(const NodeRenderingConfiguration &obj) :
            m_parameters(obj.m_parameters) {}

        NodeRenderingConfiguration& NodeRenderingConfiguration::operator=(const NodeRenderingConfiguration &obj) {
            m_parameters = obj.m_parameters;

            return (*this);
        }

        bool NodeRenderingConfiguration::hasParameter(const enum RENDERING_PARAMETERS &p) const {
            return ( (m_parameters & static_cast<uint32_t>(p)) == static_cast<uint32_t>(p));
        }

        void NodeRenderingConfiguration::setParameter(const enum RENDERING_PARAMETERS &p, const bool &enabled) {
            if (enabled) {
                m_parameters |= p;
            }
            else {
                m_parameters &= ~p;
            }
        }

    }
} // hesperia::threeD

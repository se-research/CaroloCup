/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_RENDERINGCONFIGURATION_H_
#define HESPERIA_CORE_THREED_RENDERINGCONFIGURATION_H_

#include <map>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/NodeDescriptorComparator.h"
#include "hesperia/threeD/NodeRenderingConfiguration.h"

namespace hesperia {
    namespace threeD {

        /**
         * This class configures several options for the rendering method.
         */
        class OPENDAVINCI_API RenderingConfiguration {
            public:
                RenderingConfiguration();

                /**
                 * Copy constructor.
                 *
                 * @param obj Other object.
                 */
                RenderingConfiguration(const RenderingConfiguration &obj);

                virtual ~RenderingConfiguration();

                /**
                 * Assignment operator.
                 *
                 * @param obj Other obj.
                 * @return This instance.
                 */
                RenderingConfiguration& operator=(const RenderingConfiguration &obj);

                /**
                 * This method returns the actual drawing configuration
                 * for texture.
                 *
                 * @return true if textures should be drawn.
                 */
                bool hasDrawTextures() const;

                /**
                 * This method enables or disables the drawing of textures
                 * where supported.
                 *
                 * @param drawTextures true if textures should be drawn.
                 */
                void setDrawTextures(const bool &drawTextures);

                /**
                 * This method returns a NodeRenderingConfiguration
                 * for the given NodeDescriptor.
                 *
                 * @param nd NodeDescriptor.
                 * @return NodeRenderingConfiguration.
                 */
                const NodeRenderingConfiguration& getNodeRenderingConfiguration(const NodeDescriptor &nd);

                /**
                 * This method sets a NodeRenderingConfiguration
                 * for the given NodeDescriptor.
                 *
                 * @param nd NodeDescriptor.
                 * @param nrc NodeRenderingConfiguration to be set.
                 */
                void setNodeRenderingConfiguration(const NodeDescriptor &nd, const NodeRenderingConfiguration &nrc);

            private:
                bool m_drawTextures;
                map<NodeDescriptor, NodeRenderingConfiguration, NodeDescriptorComparator> m_nodesRenderingConfiguration;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_CORE_THREED_RENDERINGCONFIGURATION_H_*/

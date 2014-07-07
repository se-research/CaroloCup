/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_RENDERER_RENDERINGCONFIGURATION_H_
#define HESPERIA_SCENEGRAPH_RENDERER_RENDERINGCONFIGURATION_H_

#include <map>

#include "core/platform.h"

#include "hesperia/scenegraph/SceneNodeDescriptor.h"
#include "hesperia/scenegraph/SceneNodeDescriptorComparator.h"
#include "hesperia/scenegraph/renderer/SceneNodeRenderingConfiguration.h"

namespace hesperia {
    namespace scenegraph {
        namespace renderer {

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
                     * @param snd SceneNodeDescriptor.
                     * @return true iff snd is already in the map.
                     */
                    bool hasSceneNodeDescriptor(const SceneNodeDescriptor &snd) const;

                    /**
                     * This method returns a SceneNodeRenderingConfiguration
                     * for the given SceneNodeDescriptor.
                     *
                     * @param snd SceneNodeDescriptor.
                     * @return SceneNodeRenderingConfiguration.
                     */
                    const SceneNodeRenderingConfiguration& getSceneNodeRenderingConfiguration(const SceneNodeDescriptor &snd);

                    /**
                     * This method sets a SceneNodeRenderingConfiguration
                     * for the given SceneNodeDescriptor.
                     *
                     * @param snd SceneNodeDescriptor.
                     * @param snrc SceneNodeRenderingConfiguration to be set.
                     */
                    void setSceneNodeRenderingConfiguration(const SceneNodeDescriptor &snd, const SceneNodeRenderingConfiguration &snrc);

                private:
                    map<SceneNodeDescriptor, SceneNodeRenderingConfiguration, SceneNodeDescriptorComparator> m_sceneNodesRenderingConfiguration;
            };

        }
    }
} // hesperia::scenegraph::renderer

#endif /*HESPERIA_SCENEGRAPH_RENDERER_RENDERINGCONFIGURATION_H_*/

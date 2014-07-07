/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_CORE_SCENEGRAPH_RENDERER_SCENENODERENDERINGCONFIGURATION_H_
#define HESPERIA_CORE_SCENEGRAPH_RENDERER_SCENENODERENDERINGCONFIGURATION_H_

#include "core/platform.h"

namespace hesperia {
    namespace scenegraph {
        namespace renderer {

            using namespace std;

            /**
             * This class configures several options for the rendering method.
             */
            class OPENDAVINCI_API SceneNodeRenderingConfiguration {
                public:
                    // Rendering parameters (must be power of two numbers since it's set using |'s).
                    enum RENDERING_PARAMETERS {
                        ENABLED = 1,
                        //NEXT_PARAMETER = 2,
                        //FURTHER_PARAMETERS = 4, ...
                    };

                public:
                    SceneNodeRenderingConfiguration();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    SceneNodeRenderingConfiguration(const SceneNodeRenderingConfiguration &obj);

                    virtual ~SceneNodeRenderingConfiguration();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    SceneNodeRenderingConfiguration& operator=(const SceneNodeRenderingConfiguration &obj);

                    /**
                     * This method returns true if the selected parameter is set.
                     *
                     * @param p Parameter to check.
                     * @return True if the selected parameter is s.
                     */
                    bool hasParameter(const enum RENDERING_PARAMETERS &p) const;

                    /**
                     * This method sets a rendering parameter.
                     *
                     * @param p Parameter to set.
                     * @param enabled True if p is set.
                     */
                    void setParameter(const enum RENDERING_PARAMETERS &p, const bool &enabled);

                private:
                    uint32_t m_parameters;
            };

        }
    }
} // hesperia::scenegraph::renderer

#endif /*HESPERIA_CORE_SCENEGRAPH_RENDERER_SCENENODERENDERINGCONFIGURATION_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PLUGINS_BIRDSEYEMAP_BIRDSEYEMAPRENDERER_H_
#define PLUGINS_BIRDSEYEMAP_BIRDSEYEMAPRENDERER_H_

#include "hesperia/scenegraph/renderer/AbstractRenderer.h"
#include "hesperia/scenegraph/renderer/RenderingConfiguration.h"

#include "QtIncludes.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            /**
             * This class is responsible for rendering scenegraph primitives.
             */
            class BirdsEyeMapRenderer : public hesperia::scenegraph::renderer::AbstractRenderer {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BirdsEyeMapRenderer(const BirdsEyeMapRenderer &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BirdsEyeMapRenderer& operator=(const BirdsEyeMapRenderer &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @painter QPainter to be used for rendering.
                     * @rC RenderingConfiguration.
                     * @pixelPerMeter Pixel per meter ratio (default: 1px = 1mm --> 1000px = 1m).
                     */
                    BirdsEyeMapRenderer(QPainter *painter, hesperia::scenegraph::renderer::RenderingConfiguration &rc, const uint32_t &pixelPerMeter = 1000);

                    virtual ~BirdsEyeMapRenderer();

                protected:
                    virtual void render(hesperia::scenegraph::primitives::Point *p);

                    virtual void render(hesperia::scenegraph::primitives::Line *l);

                    virtual void render(hesperia::scenegraph::primitives::Polygon *p);

                private:
                    QPainter *m_painter;
                    hesperia::scenegraph::renderer::RenderingConfiguration &m_renderingConfiguration;
                    uint32_t m_pixelPerMeter;
            };
        }
    }
} // plugins::birdseyemap

#endif /*PLUGINS_BIRDSEYEMAP_BIRDSEYEMAPRENDERER_H_*/

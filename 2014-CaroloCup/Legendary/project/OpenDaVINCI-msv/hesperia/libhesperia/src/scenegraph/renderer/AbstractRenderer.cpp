/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "hesperia/scenegraph/renderer/AbstractRenderer.h"

namespace hesperia {
    namespace scenegraph {
        namespace renderer {

            using namespace hesperia::scenegraph::primitives;

                AbstractRenderer::AbstractRenderer() {}

                AbstractRenderer::~AbstractRenderer() {}

                void AbstractRenderer::visit(SceneNode *sceneNode) {
                    if (sceneNode != NULL) {
                        bool delegated = false;

                        Point *p = dynamic_cast<Point*>(sceneNode);
                        if (!delegated && p != NULL) {
                            delegated = true;

                            render(p);
                        }

                        Line *l = dynamic_cast<Line*>(sceneNode);
                        if (!delegated && l != NULL) {
                            delegated = true;

                            render(l);
                        }

                        Polygon *poly = dynamic_cast<Polygon*>(sceneNode);
                        if (!delegated && poly != NULL) {
                            delegated = true;

                            render(poly);
                        }
                    }
                }

        }
    }
} // hesperia::scenegraph::renderer


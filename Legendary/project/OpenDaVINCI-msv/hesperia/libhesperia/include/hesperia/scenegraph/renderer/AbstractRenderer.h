/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_RENDERER_ABSTRACTRENDERER_H_
#define HESPERIA_SCENEGRAPH_RENDERER_ABSTRACTRENDERER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/scenegraph/primitives/Point.h"
#include "hesperia/scenegraph/primitives/Line.h"
#include "hesperia/scenegraph/primitives/Polygon.h"

namespace hesperia {
    namespace scenegraph {
        namespace renderer {

            /**
             * Interface for all SceneNodeVisitors.
             */
            class OPENDAVINCI_API AbstractRenderer : public SceneNodeVisitor {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    AbstractRenderer(const AbstractRenderer &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    AbstractRenderer& operator=(const AbstractRenderer &);

                protected:
                    AbstractRenderer();

                public:
                    virtual ~AbstractRenderer();

                    virtual void visit(SceneNode *sceneNode);

                protected:
                    /**
                     * This method needs to be implemented in sub classes to
                     * implement the rendering of a point.
                     *
                     * @param p Point to be rendered.
                     */
                    virtual void render(hesperia::scenegraph::primitives::Point *p) = 0;

                    /**
                     * This method needs to be implemented in sub classes to
                     * implement the rendering of a line.
                     *
                     * @param l Line to be rendered.
                     */
                    virtual void render(hesperia::scenegraph::primitives::Line *l) = 0;

                    /**
                     * This method needs to be implemented in sub classes to
                     * implement the rendering of a polygon.
                     *
                     * @param p Polygon to be rendered.
                     */
                    virtual void render(hesperia::scenegraph::primitives::Polygon *p) = 0;
            };

        }
    }
} // hesperia::scenegraph::renderer

#endif /*HESPERIA_SCENEGRAPH_RENDERER_ABSTRACTRENDERER_H_*/

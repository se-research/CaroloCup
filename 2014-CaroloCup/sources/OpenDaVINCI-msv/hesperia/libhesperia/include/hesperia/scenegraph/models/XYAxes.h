/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_MODELS_XYAXES_H_
#define HESPERIA_SCENEGRAPH_MODELS_XYAXES_H_

#include <vector>

#include "core/platform.h"

#include "core/data/environment/Point3.h"
#include "hesperia/scenegraph/SceneNode.h"
#include "hesperia/scenegraph/SceneNodeDescriptor.h"
#include "hesperia/scenegraph/primitives/Line.h"

namespace hesperia {
    namespace scenegraph {
        namespace models {

            using namespace std;

            /**
             * This class represents an X/Y-axes.
             */
            class OPENDAVINCI_API XYAxes : public SceneNode {
                public:
                    /**
                     * Constructor.
                     *
                     * @param sceneNodeDesciptor Description for this node.
                     * @param color Color for these X/Y-axes.
                     * @param thickness of X/Y-axes.
                     */
                    XYAxes(const SceneNodeDescriptor &sceneNodeDescriptor, const core::data::environment::Point3 &color, const float &thickness);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    XYAxes(const XYAxes &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    XYAxes& operator=(const XYAxes &obj);

                    virtual ~XYAxes();

                private:
                    core::data::environment::Point3 m_color;
                    float m_thickness;

                    void createAxes();
            };

        }
    }
} // hesperia::scenegraph::models

#endif /*HESPERIA_SCENEGRAPH_MODELS_XYAXES_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_XYZAXES_H_
#define HESPERIA_CORE_THREED_MODELS_XYZAXES_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            /**
             * This class represents the cartesian axes system.
             */
            class OPENDAVINCI_API XYZAxes : public Node {
                public:
                    /**
                     * Constructor.
                     *
                     * @param nodeDesciptor Description for this node.
                     */
                    XYZAxes(const NodeDescriptor &nodeDescriptor);

                    /**
                     * Constructor.
                     *
                     * @param lineWidth Axis' line width.
                     * @param lineLength Axis' line length.
                     */
                    XYZAxes(const NodeDescriptor &nodeDescriptor, const float &lineWidth, const float &lineLength);

                    virtual ~XYZAxes();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    XYZAxes(const XYZAxes &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    XYZAxes& operator=(const XYZAxes &obj);

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    float m_lineWidth;
                    float m_lineLength;
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_XYZAXES_H_*/

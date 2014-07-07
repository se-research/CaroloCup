/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_LINE_H_
#define HESPERIA_CORE_THREED_MODELS_LINE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace std;

            /**
             * This class represents a regular line.
             */
            class Line : public Node {
                public:
                    /**
                     * Constructor.
                     *
                    * @param nodeDesciptor Description for this node.
                     * @param positionA Point A for the line.
                     * @param positionB Point B for the line.
                     * @param color Line's color.
                     * @param width Line's width.
                     */
                    Line(const NodeDescriptor &nodeDescriptor, const core::data::environment::Point3 &positionA, const core::data::environment::Point3 &positionB, const core::data::environment::Point3 &color, const float &width);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Line(const Line &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Line& operator=(const Line &obj);

                    virtual ~Line();

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    core::data::environment::Point3 m_positionA;
                    core::data::environment::Point3 m_positionB;
                    core::data::environment::Point3 m_color;
                    float m_width;
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_LINE_H_*/

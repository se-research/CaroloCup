/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_POINT_H_
#define HESPERIA_CORE_THREED_MODELS_POINT_H_

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
             * This class represents a regular point.
             */
            class Point : public Node {
                public:
                    /**
                     * Constructor.
                     *
                     * @param position Point's position.
                     * @param color Point's color.
                     * @param width Point's width.
                     */
                    Point(const NodeDescriptor &nodeDescriptor, const core::data::environment::Point3 &position, const core::data::environment::Point3 &color, const float &width);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Point(const Point &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Point& operator=(const Point &obj);

                    virtual ~Point();

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    core::data::environment::Point3 m_position;
                    core::data::environment::Point3 m_color;
                    float m_width;
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_POINT_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_CHECKERBOARD_H_
#define HESPERIA_CORE_THREED_MODELS_CHECKERBOARD_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/threeD/Node.h"
#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace std;

            /**
             * This class represents a checker board.
             */
            class OPENDAVINCI_API CheckerBoard : public Node {
                public:
                    /**
                     * Constructor for a default checker board.
                     *
                     * @param nodeDescriptor Descriptor for this node.
                     */
                    CheckerBoard(const NodeDescriptor &nodeDescriptor);

                    /**
                     * Constructor.
                     *
                     * @param nodeDescriptor Descriptor for this node.
                     * @param positionA Position A of the checker board.
                     * @param positionB Position B of the checker board.
                     * @param height Checker board's height.
                     */
                    CheckerBoard(const NodeDescriptor &nodeDescriptor, const core::data::environment::Point3 &positionA, const core::data::environment::Point3 &positionB, const float &height);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    CheckerBoard(const CheckerBoard &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    CheckerBoard& operator=(const CheckerBoard &obj);

                    virtual ~CheckerBoard();

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    core::data::environment::Point3 m_positionA;
                    core::data::environment::Point3 m_positionB;
                    float m_height;
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_CHECKERBOARD_H_*/

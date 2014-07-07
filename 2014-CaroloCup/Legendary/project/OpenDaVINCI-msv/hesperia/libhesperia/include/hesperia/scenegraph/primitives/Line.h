/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_PRIMITIVES_LINE_H_
#define HESPERIA_SCENEGRAPH_PRIMITIVES_LINE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "hesperia/scenegraph/SceneNode.h"
#include "hesperia/scenegraph/SceneNodeDescriptor.h"

namespace hesperia {
    namespace scenegraph {
        namespace primitives {

            using namespace std;

            /**
             * This class represents a regular line.
             */
            class Line : public SceneNode {
                public:
                    /**
                     * Constructor.
                     *
                     * @param sceneNodeDesciptor Description for this scene node.
                     * @param positionA Point A for the line.
                     * @param positionB Point B for the line.
                     * @param color Line's color.
                     * @param width Line's width.
                     */
                    Line(const SceneNodeDescriptor &sceneNodeDescriptor, const core::data::environment::Point3 &positionA, const core::data::environment::Point3 &positionB, const core::data::environment::Point3 &color, const float &width);

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

                    /**
                     * @return Line's point A.
                     */
                    const core::data::environment::Point3& getA() const;

                    /**
                     * @return Line's point B.
                     */
                    const core::data::environment::Point3& getB() const;

                    /**
                     * @param A Line's point A.
                     */
                    void setA(const core::data::environment::Point3 &A);

                    /**
                     * @param B Line's point B.
                     */
                    void setB(const core::data::environment::Point3 &B);

                    /**
                     * @return Line's color.
                     */
                    const core::data::environment::Point3& getColor() const;

                    /**
                     * @return Line's width.
                     */
                    float getWidth() const;

                private:
                    core::data::environment::Point3 m_positionA;
                    core::data::environment::Point3 m_positionB;
                    core::data::environment::Point3 m_color;
                    float m_width;
            };

        }
    }
} // hesperia::threeD::primitives

#endif /*HESPERIA_SCENEGRAPH_PRIMITIVES_LINE_H_*/

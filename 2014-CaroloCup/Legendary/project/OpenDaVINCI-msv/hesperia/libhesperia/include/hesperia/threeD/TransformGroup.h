/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_TRANSFORMGROUP_H_
#define HESPERIA_CORE_THREED_TRANSFORMGROUP_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include <vector>

#include "core/base/Mutex.h"
#include "core/data/environment/Point3.h"
#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/TransformGroupVisitor.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        /**
         * This class creates a scene graph.
         */
        class OPENDAVINCI_API TransformGroup : public Node {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                TransformGroup(const TransformGroup &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                TransformGroup& operator=(const TransformGroup &);

            public:
                /**
                 * Default constructor.
                 */
                TransformGroup();

                /**
                 * Constructor.
                 *
                 * @param nodeDescriptor Description for this transform group.
                 */
                TransformGroup(const NodeDescriptor &nodeDescriptor);

                virtual ~TransformGroup();

                virtual void render(RenderingConfiguration &renderingConfiguration);

                /**
                 * This method sets the translation.
                 *
                 * @param t Translation.
                 */
                void setTranslation(const core::data::environment::Point3 &t);

                /**
                 * This method returns the translation.
                 *
                 * @return Translation.
                 */
                core::data::environment::Point3 getTranslation() const;

                /**
                 * This method sets the rotation.
                 *
                 * @param r Rotation.
                 */
                void setRotation(const core::data::environment::Point3 &r);

                /**
                 * This method returns the rotation.
                 *
                 * @return Rotation.
                 */
                core::data::environment::Point3 getRotation() const;

                /**
                 * This method sets the scaling.
                 *
                 * @param s Scaling.
                 */
                void setScaling(const core::data::environment::Point3 &s);

                /**
                 * This method returns the scaling.
                 *
                 * @return Scaling.
                 */
                core::data::environment::Point3 getScaling() const;

                /**
                 * This method adds a child.
                 *
                 * @param c Child to be added.
                 */
                void addChild(Node *c);

                /**
                 * This method removes a child.
                 *
                 * @param c Child to be removed.
                 */
                void removeChild(Node *c);

                /**
                 * This method deletes all registered children.
                 */
                void deleteAllChildren();

                /**
                 * This method accepts a visitor.
                 *
                 * @param visitor Visitor to accept.
                 */
                void accept(TransformGroupVisitor &visitor);

            private:
                core::data::environment::Point3 m_translation;
                core::data::environment::Point3 m_rotation;
                core::data::environment::Point3 m_scaling;

                mutable core::base::Mutex m_listOfChildrenMutex;
                vector<Node*> m_listOfChildren;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_CORE_THREED_TRANSFORMGROUP_H_*/

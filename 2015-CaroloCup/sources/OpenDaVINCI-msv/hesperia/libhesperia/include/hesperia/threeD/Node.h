/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_THREED_NODE_H_
#define HESPERIA_THREED_NODE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/Disposable.h"
#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/RenderingConfiguration.h"

namespace hesperia {
    namespace threeD {

        /**
         * This interface must be implemented by any subclass
         * to be drawn in an OpenGL scene.
         */
        class OPENDAVINCI_API Node : public core::wrapper::Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                Node(const Node &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                Node& operator=(const Node &);

            protected:
                Node();

            public:
                /**
                 * Constructor.
                 *
                 * @param nodeDescriptor Description of this node.
                 */
                Node(const NodeDescriptor &nodeDescriptor);

                virtual ~Node();

                /**
                 * This method is called whenever this node should to draw its
                 * content using plain OpenGL statements.
                 *
                 * @param renderingConfiguration Configuration for the rendering process.
                 */
                virtual void render(RenderingConfiguration &renderingConfiguration) = 0;

                /**
                 * This method returns this node's description.
                 *
                 * @return This node's descriptor.
                 */
                const NodeDescriptor getNodeDescriptor() const;

                /**
                 * This method sets the node descriptor.
                 *
                 * @param nodeDescriptor Node description to be set.
                 */
                void setNodeDescriptor(const NodeDescriptor &nodeDescriptor);

            private:
                NodeDescriptor m_nodeDescriptor;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_THREED_NODE_H_*/

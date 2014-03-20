/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/threeD/Node.h"

namespace hesperia {
    namespace threeD {

        Node::Node() :
            m_nodeDescriptor() {}

        Node::Node(const NodeDescriptor &nodeDescriptor) :
            m_nodeDescriptor(nodeDescriptor) {}

        Node::~Node() {}

        const NodeDescriptor Node::getNodeDescriptor() const {
            return m_nodeDescriptor;
        }

        void Node::setNodeDescriptor(const NodeDescriptor &nodeDescriptor) {
            m_nodeDescriptor = nodeDescriptor;
        }
    }
} // hesperia::threeD

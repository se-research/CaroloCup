/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/threeD/NodeDescriptor.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        NodeDescriptor::NodeDescriptor() :
                m_name() {}

        NodeDescriptor::NodeDescriptor(const string &name) :
                m_name(name) {}

        NodeDescriptor::~NodeDescriptor() {}

        NodeDescriptor::NodeDescriptor(const NodeDescriptor &obj) :
                m_name(obj.m_name) {}

        NodeDescriptor& NodeDescriptor::operator=(const NodeDescriptor &obj) {
            m_name = obj.m_name;

            return (*this);
        }

        const string NodeDescriptor::getName() const {
            return m_name;
        }
    }
} // hesperia::threeD

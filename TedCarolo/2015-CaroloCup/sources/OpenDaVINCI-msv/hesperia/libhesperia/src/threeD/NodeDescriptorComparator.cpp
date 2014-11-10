/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/threeD/NodeDescriptorComparator.h"

namespace hesperia {
    namespace threeD {

        NodeDescriptorComparator::NodeDescriptorComparator() {}

        NodeDescriptorComparator::NodeDescriptorComparator(const NodeDescriptorComparator &/*obj*/) {}

        NodeDescriptorComparator::~NodeDescriptorComparator() {}

        NodeDescriptorComparator& NodeDescriptorComparator::operator=(const NodeDescriptorComparator &/*obj*/) {
            return (*this);
        }

        bool NodeDescriptorComparator::operator()(const NodeDescriptor &nd1, const NodeDescriptor &nd2) const {
            return nd1.getName().compare(nd2.getName()) < 0;
        }

    }
} // hesperia::threeD

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/data/dmcp/ModuleDescriptorComparator.h"

namespace hesperia {
    namespace data {
        namespace dmcp {

            ModuleDescriptorComparator::ModuleDescriptorComparator() {}

            ModuleDescriptorComparator::~ModuleDescriptorComparator() {}

            bool ModuleDescriptorComparator::operator()(const ModuleDescriptor &m1, const ModuleDescriptor &m2) const {
                return m1.toString().compare(m2.toString()) < 0;
            }

        }
    }
} // hesperia::data::dmcp

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/data/dmcp/ModuleDescriptorComparator.h"

namespace core {
    namespace data {
        namespace dmcp {

            ModuleDescriptorComparator::ModuleDescriptorComparator() {}

            ModuleDescriptorComparator::~ModuleDescriptorComparator() {}

            bool ModuleDescriptorComparator::operator()(const ModuleDescriptor &m1, const ModuleDescriptor &m2) const {
                return m1.toString().compare(m2.toString()) < 0;
            }

        }
    }
} // core::data::dmcp

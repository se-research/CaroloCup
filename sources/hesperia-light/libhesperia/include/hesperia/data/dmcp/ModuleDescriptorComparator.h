/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_DMCP_MODULEDESCRIPTORCOMPARATOR_H_
#define HESPERIA_DATA_DMCP_MODULEDESCRIPTORCOMPARATOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/dmcp/ModuleDescriptor.h"

namespace hesperia {
    namespace data {
        namespace dmcp {

            using namespace std;

            /**
             * This class compares ModuleDescriptors for sorting.
             */
            class HESPERIA_API ModuleDescriptorComparator {
                public:
                    ModuleDescriptorComparator();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ModuleDescriptorComparator(const ModuleDescriptorComparator &/*obj*/) {};

                    virtual ~ModuleDescriptorComparator();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ModuleDescriptorComparator& operator=(const ModuleDescriptorComparator &/*obj*/) {
                        return (*this);
                    };

                    /**
                     * This method returns s1 < s2.
                     *
                     * @return s1 < s2.
                     */
                    bool operator()(const ModuleDescriptor &m1, const ModuleDescriptor &m2) const;
            };

        }
    }
} // hesperia::data::dmcp

#endif /*HESPERIA_DATA_DMCP_MODULEDESCRIPTORCOMPARATOR_H_*/

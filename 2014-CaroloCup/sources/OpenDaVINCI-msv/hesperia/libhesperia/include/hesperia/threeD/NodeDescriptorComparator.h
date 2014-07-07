/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_NODEDESCRIPTORCOMPARATOR_H_
#define HESPERIA_CORE_THREED_NODEDESCRIPTORCOMPARATOR_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/threeD/NodeDescriptor.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        /**
         * This class compares NodeDescriptors for sorting.
         */
        class OPENDAVINCI_API NodeDescriptorComparator {
            public:
                NodeDescriptorComparator();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                NodeDescriptorComparator(const NodeDescriptorComparator &obj);

                virtual ~NodeDescriptorComparator();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                NodeDescriptorComparator& operator=(const NodeDescriptorComparator &obj);

                /**
                 * This method returns nd1.getName().compare(nd2.getName()).
                 *
                 * @return nd1.getName().compare(nd2.getName()) < 0.
                 */
                bool operator()(const NodeDescriptor &nd1, const NodeDescriptor &nd2) const;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_CORE_THREED_NODEDESCRIPTORCOMPARATOR_H_*/

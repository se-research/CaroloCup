/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_SCENENODEDESCRIPTORCOMPARATOR_H_
#define HESPERIA_SCENEGRAPH_SCENENODEDESCRIPTORCOMPARATOR_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/scenegraph/SceneNodeDescriptor.h"

namespace hesperia {
    namespace scenegraph {

        using namespace std;

        /**
         * This class compares SceneNodeDescriptors for sorting.
         */
        class OPENDAVINCI_API SceneNodeDescriptorComparator {
            public:
                SceneNodeDescriptorComparator();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                SceneNodeDescriptorComparator(const SceneNodeDescriptorComparator &obj);

                virtual ~SceneNodeDescriptorComparator();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                SceneNodeDescriptorComparator& operator=(const SceneNodeDescriptorComparator &obj);

                /**
                 * This method returns snd1.getName().compare(snd2.getName()).
                 *
                 * @return snd1.getName().compare(snd2.getName()) < 0.
                 */
                bool operator()(const SceneNodeDescriptor &snd1, const SceneNodeDescriptor &snd2) const;
        };

    }
} // hesperia::scenegraph

#endif /*HESPERIA_SCENEGRAPH_SCENENODEDESCRIPTORCOMPARATOR_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_SCENENODEDESCRIPTOR_H_
#define HESPERIA_SCENEGRAPH_SCENENODEDESCRIPTOR_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace scenegraph {

        using namespace std;

        /**
         * This class provides a description for the current scene node.
         */
        class OPENDAVINCI_API SceneNodeDescriptor {
            public:
                SceneNodeDescriptor();

                /**
                 * Constructor.
                 *
                 * @param name Name of this scene node descriptor.
                 */
                SceneNodeDescriptor(const string &name);

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                SceneNodeDescriptor(const SceneNodeDescriptor &obj);

                virtual ~SceneNodeDescriptor();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                SceneNodeDescriptor& operator=(const SceneNodeDescriptor &obj);

                /**
                 * This method returns the name of this node.
                 *
                 * @return Name of this node.
                 */
                const string getName() const;

            private:
                string m_name;
        };

    }
} // hesperia::scenegraph

#endif /*HESPERIA_SCENEGRAPH_SCENENODEDESCRIPTOR_H_*/

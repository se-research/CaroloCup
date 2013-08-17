/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPH_TRANSFORMATION_SCENEGRAPHFACTORY_H_
#define HESPERIA_SCENEGRAPH_TRANSFORMATION_SCENEGRAPHFACTORY_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Mutex.h"
#include "hesperia/scenario/SCNXArchive.h"
#include "hesperia/scenegraph/SceneNode.h"
#include "hesperia/scenegraph/SceneNodeDescriptor.h"

namespace hesperia {
    namespace scenegraph {
        namespace transformation {

            using namespace std;

            /**
             * This class creates an appropriate visualization for a given SCNX file.
             */
            class OPENDAVINCI_API SceneGraphFactory {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    SceneGraphFactory(const SceneGraphFactory &);
                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    SceneGraphFactory& operator=(const SceneGraphFactory &);

                private:
                    SceneGraphFactory();

                public:
                    virtual ~SceneGraphFactory();

                    /**
                     * This method returns a static instance for this factory.
                     *
                     * @return Instance of this factory.
                     */
                    static SceneGraphFactory& getInstance();

                    /**
                     * This method returns a renderable scenegraph
                     * node for the given data structure.
                     *
                     * @param scnxArchive SCNXArchive to be visualized.
                     * @return Renderable node or NULL.
                     */
                    SceneNode* transform(scenario::SCNXArchive &scnxArchive);

                    /**
                     * This method returns a renderable scenegraph
                     * node for the given data structure.
                     *
                     * @param scnxArchive SCNXArchive to be visualized.
                     * @param showLaneConnectors if true, the red connectors between lane segments will be rendered.
                     * @return Renderable node or NULL.
                     */
                    SceneNode* transform(scenario::SCNXArchive &scnxArchive, const bool &showLaneConnectors);

                private:
                    static core::base::Mutex m_singletonMutex;
                    static SceneGraphFactory* m_singleton;
            };

        }
    }
} // hesperia::scenegraph::transformation

#endif /*HESPERIA_SCENEGRAPH_TRANSFORMATION_SCENEGRAPHFACTORY_H_*/

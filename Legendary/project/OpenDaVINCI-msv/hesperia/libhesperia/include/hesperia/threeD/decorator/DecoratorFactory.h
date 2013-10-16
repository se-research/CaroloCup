/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_DECORATOR_DECORATORFACTORY_H_
#define HESPERIA_CORE_THREED_DECORATOR_DECORATORFACTORY_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Mutex.h"
#include "hesperia/scenario/SCNXArchive.h"
#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/loaders/OBJXArchive.h"

namespace hesperia {
    namespace threeD {
        namespace decorator {

            using namespace std;

            /**
             * This class creates an appropriate visualization for a given datatype.
             */
            class OPENDAVINCI_API DecoratorFactory {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    DecoratorFactory(const DecoratorFactory &);
                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    DecoratorFactory& operator=(const DecoratorFactory &);

                private:
                    DecoratorFactory();

                public:
                    virtual ~DecoratorFactory();

                    /**
                     * This method returns a static instance for this factory.
                     *
                     * @return Instance of this factory.
                     */
                    static DecoratorFactory& getInstance();

                    /**
                     * This method returns an OpenGL-displayable scenegraph
                     * node for the given data structure.
                     *
                     * @param scnxArchive SCNXArchive to be visualized.
                     * @return OpenGL displayable node or NULL.
                     */
                    Node* decorate(scenario::SCNXArchive &scnxArchive);

                    /**
                     * This method returns an OpenGL-displayable scenegraph
                     * node for the given data structure.
                     *
                     * @param scnxArchive SCNXArchive to be visualized.
                     * @param showLaneConnectors if true, the red connectors between lane segments will be rendered.
                     * @return OpenGL displayable node or NULL.
                     */
                    Node* decorate(scenario::SCNXArchive &scnxArchive, const bool &showLaneConnectors);

                    /**
                     * This method returns an OpenGL-displayable scenegraph
                     * node for the given data structure.
                     *
                     * @param objxArchive OBJXArchive to be visualized.
                     * @param nd NodeDescriptor.
                     * @return OpenGL displayable node or NULL.
                     */
                    Node* decorate(loaders::OBJXArchive &objxArchive, const NodeDescriptor &nd);

                private:
                    static core::base::Mutex m_singletonMutex;
                    static DecoratorFactory* m_singleton;
            };

        }
    }
} // hesperia::threeD::decorator

#endif /*HESPERIA_CORE_THREED_DECORATOR_DECORATORFACTORY_H_*/

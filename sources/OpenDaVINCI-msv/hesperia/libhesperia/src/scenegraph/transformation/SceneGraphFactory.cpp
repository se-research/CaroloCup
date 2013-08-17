/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/macros.h"
#include "core/base/Lock.h"

#include "hesperia/scenegraph/SceneNodeDescriptor.h"
#include "hesperia/scenegraph/transformation/SceneGraphFactory.h"
#include "hesperia/scenegraph/transformation/ScenarioTransformation.h"

namespace hesperia {
    namespace scenegraph {
        namespace transformation {

            using namespace std;
            using namespace core::base;
            using namespace core::data::environment;
            using namespace data::scenario;
            using namespace scenario;

            // Initialize singleton instance.
            Mutex SceneGraphFactory::m_singletonMutex;
            SceneGraphFactory* SceneGraphFactory::m_singleton = NULL;

            SceneGraphFactory::SceneGraphFactory() {}

            SceneGraphFactory::~SceneGraphFactory() {
                SceneGraphFactory::m_singleton = NULL;
            }

            SceneGraphFactory& SceneGraphFactory::getInstance() {
                {
                    Lock l(SceneGraphFactory::m_singletonMutex);
                    if (SceneGraphFactory::m_singleton == NULL) {
                        SceneGraphFactory::m_singleton = new SceneGraphFactory();
                    }
                }

                return (*SceneGraphFactory::m_singleton);
            }

            SceneNode* SceneGraphFactory::transform(scenario::SCNXArchive &scnxArchive) {
                return transform(scnxArchive, true);
            }

            SceneNode* SceneGraphFactory::transform(scenario::SCNXArchive &scnxArchive, const bool &showLaneConnectors) {
                SceneNode *sn = new SceneNode();

                if (scnxArchive.getAerialImage() != NULL) {
                    clog << "SceneGraphFactory: Found AerialImage but currently no transformation available." << endl;
                }

                ScenarioTransformation scenarioTransformer(showLaneConnectors);

                Scenario &scenario = scnxArchive.getScenario();
                scenario.accept(scenarioTransformer);

                sn->addChild(scenarioTransformer.getRoot());

                return sn;
            }

        }
    }
} // hesperia::scenegraph::transformation

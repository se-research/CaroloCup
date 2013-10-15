/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_SCENARIONODE_H_
#define HESPERIA_CORE_DATA_SCENARIO_SCENARIONODE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            // Forward declaration to prevent circular dependencies.
            class ScenarioVisitor;

            /**
             * This interface allows the use of visitors for transforming
             * the scenario data structure.
             */
            class OPENDAVINCI_API ScenarioNode {
                public:
                    virtual ~ScenarioNode();

                    /**
                     * This method accepts a visitor for traversing the scenario
                     * to build graphs or 3D scenes.
                     *
                     * @param visitor Visitor to be accepted.
                     */
                    virtual void accept(ScenarioVisitor &visitor) = 0;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_SCENARIONODE_H_*/

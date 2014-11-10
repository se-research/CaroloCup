/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_SCENARIOVISITOR_H_
#define HESPERIA_CORE_DATA_SCENARIO_SCENARIOVISITOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/scenario/ScenarioNode.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            /**
             * This interface allows the use of visitors for transforming
             * the scenario data structure.
             */
            class OPENDAVINCI_API ScenarioVisitor {
                public:
                    virtual ~ScenarioVisitor();

                    /**
                     * This method visits a node.
                     *
                     * @param node Node to be visited.
                     */
                    virtual void visit(ScenarioNode &node) = 0;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_DATA_CORE_SCENARIO_SCENARIOVISITOR_H_*/

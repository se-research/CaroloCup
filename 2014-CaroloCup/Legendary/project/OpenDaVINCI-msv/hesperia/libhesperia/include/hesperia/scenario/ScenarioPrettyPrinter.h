/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_SCENARIO_SCENARIOPRETTYPRINTER_H_
#define HESPERIA_SCENARIO_SCENARIOPRETTYPRINTER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/scenario/ScenarioVisitor.h"

namespace hesperia {
    namespace scenario {

        using namespace std;

        /**
         * This class pretty prints the Scenario data structure.
         */
        class OPENDAVINCI_API ScenarioPrettyPrinter : public data::scenario::ScenarioVisitor {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ScenarioPrettyPrinter(const ScenarioPrettyPrinter &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ScenarioPrettyPrinter& operator=(const ScenarioPrettyPrinter &);

            public:
                ScenarioPrettyPrinter();

                virtual ~ScenarioPrettyPrinter();

                virtual void visit(data::scenario::ScenarioNode &node);
        };

    }
} // hesperia::scenario

#endif /*HESPERIA_SCENARIO_SCENARIOPRETTYPRINTER_H_*/

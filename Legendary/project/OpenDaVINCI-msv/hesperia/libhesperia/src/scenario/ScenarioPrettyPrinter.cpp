/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>
#include <string>

#include "hesperia/scenario/ScenarioPrettyPrinter.h"

namespace hesperia {
    namespace scenario {

        using namespace std;
        using namespace core::data;
        using namespace data::scenario;

        ScenarioPrettyPrinter::ScenarioPrettyPrinter() {}

        ScenarioPrettyPrinter::~ScenarioPrettyPrinter() {}

        void ScenarioPrettyPrinter::visit(ScenarioNode &node) {
            try {
                SerializableData &s = dynamic_cast<SerializableData&>(node);
                clog << s.toString() << endl;
            } catch (...) {}
        }

    }
} // hesperia::scenario

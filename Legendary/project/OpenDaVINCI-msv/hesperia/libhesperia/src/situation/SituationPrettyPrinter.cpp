/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>
#include <string>

#include "hesperia/situation/SituationPrettyPrinter.h"

namespace hesperia {
    namespace situation {

        using namespace std;
        using namespace core::data;
        using namespace data::situation;

        SituationPrettyPrinter::SituationPrettyPrinter() {}

        SituationPrettyPrinter::~SituationPrettyPrinter() {}

        void SituationPrettyPrinter::visit(SituationNode &node) {
            try {
                SerializableData &s = dynamic_cast<SerializableData&>(node);
                clog << s.toString() << endl;
            } catch (...) {}
        }

    }
} // hesperia::situation

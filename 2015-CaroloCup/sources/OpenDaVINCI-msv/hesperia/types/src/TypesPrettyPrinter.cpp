/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <iostream>
#include <string>

#include "TypesPrettyPrinter.h"

namespace types {

    using namespace std;
    using namespace core::data;

    SituationPrettyPrinter::SituationPrettyPrinter() {}

    SituationPrettyPrinter::~SituationPrettyPrinter() {}

    void SituationPrettyPrinter::visit(hesperia::data::situation::SituationNode &/*node*/) {
//        try {
//            SerializableData &s = dynamic_cast<SerializableData&>(node);
//            clog << s.toString() << endl;
//        } catch (...) {}
    }

} // types

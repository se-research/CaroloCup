/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <iostream>
#include <sstream>

#include "TypesFactory.h"
#include "Types.h"

namespace types {

    using namespace std;

    Types::Types(const int32_t &argc, char **argv) {
        cout << argc << " " << *argv << endl;
    }

    Types::~Types() {}

    int32_t Types::run() {
        cout << "Type compiler" << endl;

        stringstream s;
        s << "Point3 {" << endl;
        s << " int32_t x;" << endl;
        s << " int32_t y;" << endl;
        s << " int32_t z;" << endl;
        s << "};" << endl;

        try {
            TypesFactory::getInstance().getSituation(s.str());
        } catch (...) {
            //failed = (iae.toString() != "InvalidArgumentException: SITGrammarErrorListener caught an unknown parser error at line: 3 in context \'SITUATION Test-Situation\nVERSION v1.0\n===Erroneous line===>>> DATE3 July-15-2008\nSCENARIO Test-Szenario\n\'. at build/libhesperia/testing/src/situation/SituationFactory.cpp: 60");
            cout << "Invalid data structure." << endl;
        }

        return 0;
    }

} // types

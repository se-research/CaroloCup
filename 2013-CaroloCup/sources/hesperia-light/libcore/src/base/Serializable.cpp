/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <string>

#include "core/base/Serializable.h"

namespace core {
    namespace base {

        Serializable::~Serializable() {}

    }
} // core::base

namespace std {

    ostream &operator<<(ostream &out, const core::base::Serializable &s) {
        // TODO: Move to serialization inside the Serializable.

        // Write version number and endl.
//        out << s.getVersion() << endl;
        return s.operator << (out);
    }

    istream &operator>>(istream &in, core::base::Serializable &s) {
        // TODO: Move to deserialization inside the Serializable.

        // Consume version number and endl.
//        string version; in >> version; in.get();
//        s.setDeserializedVersion(version);
        return s.operator >> (in);
    }

}

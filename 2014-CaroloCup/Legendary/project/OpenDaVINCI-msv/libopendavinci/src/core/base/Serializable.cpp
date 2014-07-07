/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Serializable.h"

namespace core {
    namespace base {

        Serializable::~Serializable() {}

    }
} // core::base

namespace std {

    ostream &operator<<(ostream &out, const core::base::Serializable &s) {
        return s.operator << (out);
    }

    istream &operator>>(istream &in, core::base::Serializable &s) {
        return s.operator >> (in);
    }

}

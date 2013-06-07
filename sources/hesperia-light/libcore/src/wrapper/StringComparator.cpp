/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/StringComparator.h"

namespace core {
    namespace wrapper {

        StringComparator::StringComparator() {}

        StringComparator::StringComparator(const StringComparator &/*obj*/) {}

        StringComparator::~StringComparator() {}

        StringComparator& StringComparator::operator=(const StringComparator &/*obj*/) {
            return (*this);
        }

        bool StringComparator::operator()(const string& s1, const string& s2) const {
            return s1.compare(s2) < 0;
        }

    }
} // core::wrapper

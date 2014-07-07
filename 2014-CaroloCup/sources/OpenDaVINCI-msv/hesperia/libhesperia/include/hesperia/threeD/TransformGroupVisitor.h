/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_TRANSFORMGROUPVISITOR_H_
#define HESPERIA_CORE_THREED_TRANSFORMGROUPVISITOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/threeD/Node.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        /**
         * This interface can be implemented to traverse  a NodeTree built by TransformGroups.
         */
        class OPENDAVINCI_API TransformGroupVisitor {
            public:
                virtual ~TransformGroupVisitor();

                virtual void visit(Node *nd) = 0;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_CORE_THREED_TRANSFORMGROUPVISITOR_H_*/

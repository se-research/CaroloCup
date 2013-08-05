/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_SITUATIONNODE_H_
#define HESPERIA_CORE_DATA_SITUATION_SITUATIONNODE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace data {
        namespace situation {

            // Forward declaration to prevent circular dependencies.
            class SituationVisitor;

            /**
             * This interface allows the use of visitors for transforming
             * the situation data structure.
             */
            class OPENDAVINCI_API SituationNode {
                public:
                    virtual ~SituationNode();

                    /**
                     * This method accepts a visitor for traversing the situation.
                     *
                     * @param visitor Visitor to be accepted.
                     */
                    virtual void accept(SituationVisitor &visitor) = 0;
            };

        }
    }
} // hesperia::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_SITUATIONNODE_H_*/

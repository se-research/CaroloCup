/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_SITUATIONVISITOR_H_
#define HESPERIA_CORE_DATA_SITUATION_SITUATIONVISITOR_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/situation/SituationNode.h"

namespace hesperia {
    namespace data {
        namespace situation {

            /**
             * This interface allows the use of visitors for transforming
             * the situation data structure.
             */
            class OPENDAVINCI_API SituationVisitor {
                public:
                    virtual ~SituationVisitor();

                    /**
                     * This method visits a node.
                     *
                     * @param node Node to be visited.
                     */
                    virtual void visit(SituationNode &node) = 0;
            };

        }
    }
} // hesperia::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_SITUATIONVISITOR_H_*/

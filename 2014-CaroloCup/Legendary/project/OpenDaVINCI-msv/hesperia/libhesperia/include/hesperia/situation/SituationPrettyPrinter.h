/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_SITUATION_SITUATIONPRETTYPRINTER_H_
#define HESPERIA_CORE_SITUATION_SITUATIONPRETTYPRINTER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/situation/SituationVisitor.h"

namespace hesperia {
    namespace situation {

        using namespace std;

        /**
         * This class pretty prints the Situation data structure.
         */
        class OPENDAVINCI_API SituationPrettyPrinter : public data::situation::SituationVisitor {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SituationPrettyPrinter(const SituationPrettyPrinter &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SituationPrettyPrinter& operator=(const SituationPrettyPrinter &);

            public:
                SituationPrettyPrinter();

                virtual ~SituationPrettyPrinter();

                virtual void visit(data::situation::SituationNode &node);
        };

    }
} // hesperia::situation

#endif /*HESPERIA_CORE_SITUATION_SITUATIONPRETTYPRINTER_H_*/

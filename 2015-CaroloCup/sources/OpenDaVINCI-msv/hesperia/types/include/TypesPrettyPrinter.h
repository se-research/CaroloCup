/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#ifndef TYPESPRETTYPRINTER_H_
#define TYPESPRETTYPRINTER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "hesperia/data/situation/SituationNode.h"
#include "TypesVisitor.h"

namespace types {

    using namespace std;

    /**
     * This class pretty prints the Situation data structure.
     */
    class OPENDAVINCI_API SituationPrettyPrinter : public hesperia::data::situation::SituationVisitor {
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

            virtual void visit(hesperia::data::situation::SituationNode &node);
    };

} // types

#endif /*TYPESPRETTYPRINTER_H_*/

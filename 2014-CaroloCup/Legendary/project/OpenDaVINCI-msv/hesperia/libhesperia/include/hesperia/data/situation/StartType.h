/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_STARTTYPE_H_
#define HESPERIA_CORE_DATA_SITUATION_STARTTYPE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"
#include "hesperia/data/situation/SituationNode.h"
#include "hesperia/data/situation/SituationVisitor.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents an abstract start type (IMMEDIATELY, ONMOVING, ONENTERINGPOLYGON).
             */
            class OPENDAVINCI_API StartType : public core::data::SerializableData, public SituationNode {
                public:
                    enum STARTTYPE {
                        UNDEFINED,
                        IMMEDIATELY,
                        ONMOVING,
                        ONENTERINGPOLYGON
                    };

                protected:
                    StartType();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    StartType(const StartType &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    StartType& operator=(const StartType &obj);

                public:
                    virtual ~StartType();

                    virtual void accept(SituationVisitor &visitor) = 0;

                    /**
                     * This method returns the type of the starttype.
                     *
                     * @return Type of starttype.
                     */
                    enum STARTTYPE getType() const;

                    /**
                     * This method sets the starttype's type.
                     *
                     * @param type Type of the starttype.
                     */
                    void setType(const enum StartType::STARTTYPE &type);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    enum STARTTYPE m_type;
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_STARTTYPE_H_*/

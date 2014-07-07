/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_BEHAVIOR_H_
#define HESPERIA_CORE_DATA_SITUATION_BEHAVIOR_H_

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
             * This class represents an abstract behavior (i.e. EXTERNALDRIVER or
             * POINTIDDRIVER at the moment).
             */
            class OPENDAVINCI_API Behavior : public core::data::SerializableData, public SituationNode {
                public:
                    enum BEHAVIORTYPE {
                        UNDEFINED,
                        EXTERNALDRIVER,
                        POINTIDDRIVER
                    };

                protected:
                    Behavior();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Behavior(const Behavior &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Behavior& operator=(const Behavior &obj);

                public:
                    virtual ~Behavior();

                    virtual void accept(SituationVisitor &visitor) = 0;

                    /**
                     * This method returns the type of the behavior.
                     *
                     * @return Type of behavior.
                     */
                    enum BEHAVIORTYPE getType() const;

                    /**
                     * This method sets the behavior's type.
                     *
                     * @param type Type of the behavior.
                     */
                    void setType(const enum Behavior::BEHAVIORTYPE &type);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    enum BEHAVIORTYPE m_type;
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_BEHAVIOR_H_*/

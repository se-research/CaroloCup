/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_STOPTYPE_H_
#define HESPERIA_CORE_DATA_SITUATION_STOPTYPE_H_

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
             * This class represents an abstract stop type (STOP, RETURNTOSTART, WARPTOSTART).
             */
            class OPENDAVINCI_API StopType : public core::data::SerializableData, public SituationNode {
                public:
                    enum STOPTYPE {
                        UNDEFINED,
                        STOP,
                        RETURNTOSTART,
                        WARPTOSTART
                    };

                protected:
                    StopType();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    StopType(const StopType &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    StopType& operator=(const StopType &obj);

                public:
                    virtual ~StopType();

                    virtual void accept(SituationVisitor &visitor) = 0;

                    /**
                     * This method returns the type of the stoptype.
                     *
                     * @return Type of stoptype.
                     */
                    enum STOPTYPE getType() const;

                    /**
                     * This method sets the stoptype type.
                     *
                     * @param type Type of the stoptype.
                     */
                    void setType(const enum StopType::STOPTYPE &type);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    enum STOPTYPE m_type;
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_STOPTYPE_H_*/

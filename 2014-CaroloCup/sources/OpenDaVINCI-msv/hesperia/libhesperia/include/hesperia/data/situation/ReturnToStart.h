/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_RETURNTOSTART_H_
#define HESPERIA_CORE_DATA_SITUATION_RETURNTOSTART_H_

#include "hesperia/data/situation/StopType.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents the start type STOP.
             */
            class OPENDAVINCI_API ReturnToStart : public StopType {
                public:
                    ReturnToStart();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ReturnToStart(const ReturnToStart &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ReturnToStart& operator=(const ReturnToStart &obj);

                    virtual ~ReturnToStart();

                    virtual void accept(SituationVisitor &visitor);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_RETURNTOSTART_H_*/

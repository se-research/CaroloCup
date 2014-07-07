/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_EXTERNALDRIVER_H_
#define HESPERIA_CORE_DATA_SITUATION_EXTERNALDRIVER_H_

#include "hesperia/data/situation/Behavior.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents an external driver.
             */
            class OPENDAVINCI_API ExternalDriver : public Behavior {
                public:
                    ExternalDriver();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    ExternalDriver(const ExternalDriver &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    ExternalDriver& operator=(const ExternalDriver &obj);

                    virtual ~ExternalDriver();

                    virtual void accept(SituationVisitor &visitor);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_EXTERNALDRIVER_H_*/

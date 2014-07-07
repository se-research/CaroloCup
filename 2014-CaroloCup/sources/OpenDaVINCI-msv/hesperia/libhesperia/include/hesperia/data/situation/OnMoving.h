/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_ONMOVING_H_
#define HESPERIA_CORE_DATA_SITUATION_ONMOVING_H_

#include "hesperia/data/situation/StartType.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents the start type ONMOVING.
             */
            class OPENDAVINCI_API OnMoving : public StartType {
                public:
                    OnMoving();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    OnMoving(const OnMoving &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    OnMoving& operator=(const OnMoving &obj);

                    virtual ~OnMoving();

                    virtual void accept(SituationVisitor &visitor);

                    /**
                     * This method returns the object's ID.
                     *
                     * @return Object's ID.
                     */
                    uint32_t getID() const;

                    /**
                     * This method sets the object's ID.
                     *
                     * @param id Object's ID.
                     */
                    void setID(const uint32_t &id);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_id;
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_ONMOVING_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_WARPTOSTART_H_
#define HESPERIA_CORE_DATA_SITUATION_WARPTOSTART_H_

#include "hesperia/data/situation/StopType.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents the start type STOP.
             */
            class OPENDAVINCI_API WarpToStart : public StopType {
                public:
                    WarpToStart();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    WarpToStart(const WarpToStart &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    WarpToStart& operator=(const WarpToStart &obj);

                    virtual ~WarpToStart();

                    virtual void accept(SituationVisitor &visitor);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_WARPTOSTART_H_*/

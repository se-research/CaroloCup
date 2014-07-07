/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_IDVERTEX3_H_
#define HESPERIA_CORE_DATA_SCENARIO_IDVERTEX3_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/scenario/Vertex3.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents an identifiable two dimensional coordinate.
             */
            class OPENDAVINCI_API IDVertex3 : public Vertex3 {
                public:
                    IDVertex3();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    IDVertex3(const IDVertex3 &obj);

                    virtual ~IDVertex3();

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    IDVertex3& operator=(const IDVertex3 &obj);

                    /**
                     * This method returns the ID.
                     *
                     * @return ID.
                     */
                    uint32_t getID() const;

                    /**
                     * This method sets the ID.
                     *
                     * @param ID.
                     */
                    void setID(const uint32_t &ID);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_id;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_IDVERTEX3_H_*/

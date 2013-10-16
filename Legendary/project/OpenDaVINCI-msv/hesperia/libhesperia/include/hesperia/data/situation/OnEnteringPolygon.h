/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SITUATION_ONENTERINGPOLYGON_H_
#define HESPERIA_CORE_DATA_SITUATION_ONENTERINGPOLYGON_H_

#include <vector>

#include "hesperia/data/situation/StartType.h"
#include "hesperia/data/situation/Vertex3.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;

            /**
             * This class represents the start type ONENTERINGPOLYGON.
             */
            class OPENDAVINCI_API OnEnteringPolygon : public StartType {
                public:
                    OnEnteringPolygon();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    OnEnteringPolygon(const OnEnteringPolygon &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    OnEnteringPolygon& operator=(const OnEnteringPolygon &obj);

                    virtual ~OnEnteringPolygon();

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

                    /**
                     * This method returns the list of vertices.
                     *
                     * @return List of vertices.
                     */
                    const vector<Vertex3>& getListOfVertices() const;

                    /**
                     * This method add a new vertex to the polygon.
                     *
                     * @param v Vertex to be added.
                     */
                    void add(const Vertex3 &v);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    uint32_t m_id;
                    vector<Vertex3> m_listOfVertices;

                    /**
                     * This method creates a deep copy.
                     *
                     * @param obj Object to be copy.
                     */
                    void deepCopy(const OnEnteringPolygon &obj);
            };

        }
    }
} // core::data::situation

#endif /*HESPERIA_CORE_DATA_SITUATION_ONENTERINGPOLYGON_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_WGS84COORDINATESYSTEM_H_
#define HESPERIA_CORE_DATA_SCENARIO_WGS84COORDINATESYSTEM_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/scenario/CoordinateSystem.h"
#include "hesperia/data/scenario/Vertex3.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents a two-dimensional WGS84-coordinate
             * system to be used of a scenario.
             */
            class OPENDAVINCI_API WGS84CoordinateSystem : public CoordinateSystem {
                public:
                    WGS84CoordinateSystem();

                    virtual ~WGS84CoordinateSystem();

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    WGS84CoordinateSystem(const WGS84CoordinateSystem &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    WGS84CoordinateSystem& operator=(const WGS84CoordinateSystem &obj);

                    /**
                     * This method returns the origin of the WGS84 coordinate system.
                     *
                     * @return Origin of the coordinate system.
                     */
                    const Vertex3 getOrigin() const;

                    /**
                     * This method sets the coordinate system's origin.
                     *
                     * @param o Origin of the coordinate system.
                     */
                    void setOrigin(const Vertex3 &o);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    Vertex3 m_origin;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_WGS84COORDINATESYSTEM_H_*/

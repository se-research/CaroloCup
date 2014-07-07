/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_COORDINATESYSTEM_H_
#define HESPERIA_CORE_DATA_SCENARIO_COORDINATESYSTEM_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/scenario/ScenarioNode.h"
#include "hesperia/data/scenario/ScenarioVisitor.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents the coordinate system to be used of a scenario.
             */
            class OPENDAVINCI_API CoordinateSystem : public core::data::SerializableData, public ScenarioNode {
                protected:
                    CoordinateSystem();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    CoordinateSystem(const CoordinateSystem &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    CoordinateSystem& operator=(const CoordinateSystem &obj);

                public:
                    virtual ~CoordinateSystem();

                    virtual void accept(ScenarioVisitor &visitor) = 0;

                    /**
                     * This method returns the type of the coordinate system.
                     *
                     * @return Type of coordinate system.
                     */
                    const string getType() const;

                    /**
                     * This method sets the coordinate system's type.
                     *
                     * @param type Type of the coordinate system.
                     */
                    void setType(const string &type);

                    /**
                     * This method returns the coordinate system's rotation.
                     *
                     * @return Rotation of the coordinate system.
                     */
                    double getRotation() const;

                    /**
                     * This method sets the coordinate system's rotation.
                     *
                     * @param r Rotation of the coordinate system.
                     */
                    void setRotation(const double &r);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    string m_type;
                    double m_rotation;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_COORDINATESYSTEM_H_*/

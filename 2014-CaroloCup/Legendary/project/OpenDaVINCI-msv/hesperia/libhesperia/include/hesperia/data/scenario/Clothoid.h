/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_CLOTHOID_H_
#define HESPERIA_CORE_DATA_SCENARIO_CLOTHOID_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/SerializableData.h"

#include "hesperia/data/scenario/StraightLine.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents a clothoid.
             */
            class OPENDAVINCI_API Clothoid : public StraightLine {
                public:
                    Clothoid();

                    virtual ~Clothoid();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Clothoid(const Clothoid &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Clothoid& operator=(const Clothoid &obj);

                    virtual void accept(scenario::ScenarioVisitor &visitor);

                    /**
                     * This method returns the curvature change dk.
                     *
                     * @return dk.
                     */
                    double getDK() const;

                    /**
                     * This method sets the curvature change dk.
                     *
                     * @param dk Curvature change.
                     */
                    void setDK(const double &dk);

                    /**
                     * This method returns the curvature k.
                     *
                     * @return k.
                     */
                    double getK() const;

                    /**
                     * This method sets the curvature k.
                     *
                     * @param k Curvature.
                     */
                    void setK(const double &k);

                    /**
                     * This method returns the rotation around the z-axis.
                     *
                     * @return Rotation around the z-axis.
                     */
                    double getRotationZ() const;

                    /**
                     * This method sets the rotation around the z-axis.
                     *
                     * @param rotationZ Rotation around z-axis.
                     */
                    void setRotationZ(const double &rotationZ);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    double m_dk; // Curvature change.
                    double m_k; // Curvature (1/r).
                    double m_rotationZ;
            };

        }
    }
} // hesperia::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_CLOTHOID_H_*/

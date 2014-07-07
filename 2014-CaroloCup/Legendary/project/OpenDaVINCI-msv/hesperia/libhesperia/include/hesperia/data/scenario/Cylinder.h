/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DATA_SCENARIO_CYLINDER_H_
#define HESPERIA_CORE_DATA_SCENARIO_CYLINDER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/data/scenario/Shape.h"
#include "hesperia/data/scenario/Vertex3.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;

            /**
             * This class represents a cylinder.
             */
            class OPENDAVINCI_API Cylinder : public Shape {
                public:
                    Cylinder();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Cylinder(const Cylinder &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Cylinder& operator=(const Cylinder &obj);

                    virtual ~Cylinder();

                    virtual void accept(ScenarioVisitor &visitor);

                    /**
                     * This method returns the center of this cylinder.
                     *
                     * @return Center.
                     */
                    const Vertex3& getCenter() const;

                    /**
                     * This method sets the center of this cylinder.
                     *
                     * @param c Center.
                     */
                    void setCenter(const Vertex3 &c);

                    /**
                     * This method returns the radius of this cylinder.
                     *
                     * @return Radius.
                     */
                    double getRadius() const;

                    /**
                     * This method sets the radius of this cylinder.
                     * Radius must be greater than 0.
                     *
                     * @param r Radius.
                     */
                    void setRadius(const double &r);

                    /**
                     * This method returns the height of this cylinder.
                     *
                     * @return Height.
                     */
                    double getHeight() const;

                    /**
                     * This method sets the height of this cylinder.
                     *
                     * @param h Height.
                     */
                    void setHeight(const double &h);

                    /**
                     * This method returns the color (R, G, B) of this cylinder.
                     *
                     * @return Color.
                     */
                    const Vertex3& getColor() const;

                    /**
                     * This method sets the color of this cylinder.
                     *
                     * @param c Color.
                     */
                    void setColor(const Vertex3 &c);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    Vertex3 m_center;
                    double m_radius;
                    double m_height;
                    Vertex3 m_color;
            };

        }
    }
} // core::data::scenario

#endif /*HESPERIA_CORE_DATA_SCENARIO_CYLINDER_H_*/

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_ENVIRONMENT_LINE_H_
#define HESPERIA_DATA_ENVIRONMENT_LINE_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"
#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;

            using namespace core::data::environment;

            /**
             * This class can be used for line operations.
             */
            class OPENDAVINCI_API Line : public core::data::SerializableData {
                private:
                    const static double EPSILON;

                public:
                    Line();

                    /**
                     * Constructor.
                     *
                     * @param A
                     * @param B
                     */
                    Line(const Point3 &A, const Point3 &B);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    Line(const Line &obj);

                    virtual ~Line();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    Line& operator=(const Line &obj);

                    /**
                     * This method returns point A.
                     *
                     * @return Point A.
                     */
                    Point3 getA() const;

                    /**
                     * This method sets point A.
                     *
                     * @param A Point A.
                     */
                    void setA(const Point3 &A);

                    /**
                     * This method returns point B.
                     *
                     * @return Point B.
                     */
                    Point3 getB() const;

                    /**
                     * This method sets point B.
                     *
                     * @param B Point B.
                     */
                    void setB(const Point3 &B);

                    /**
                     * This method computes the intersection point
                     * ignoring the Z coordinate.
                     *
                     * @param l Line to intersect.
                     * @param result Result of intersection.
                     * @return true, if an intersection point could be found.
                     */
                    bool intersectIgnoreZ(const Line &l, Point3 &result) const;

                    /**
                     * This method computes the perpendicular point
                     * for the given p.
                     *
                     * @param p Point to be used for perpendicular computation.
                     * @return Perpendicular point.
                     */
                    const Point3 getPerpendicularPoint(const Point3 &p) const;

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    Point3 m_A;
                    Point3 m_B;

                    double det(const double &a, const double &b, const double &c, const double &d) const;
            };

        }
    }
} // hesperia::data::environment

#endif /*HESPERIA_DATA_ENVIRONMENT_LINE_H_*/

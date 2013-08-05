/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_MATH_TRANSFORMATION_H_
#define HESPERIA_MATH_TRANSFORMATION_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"
#include "core/data/environment/Position.h"

namespace hesperia {
    namespace math {

        /**
         * This class implements different transformation operations.
         */
        class OPENDAVINCI_API Transformation {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                Transformation(const Transformation &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                Transformation& operator=(const Transformation &);

            public:
                Transformation();

                virtual ~Transformation();

                /**
                 * This method transforms the given cartesian coordinate from
                 * the position coordinate system to the world coordinate system.
                 *
                 * @param coordinate Coordinate to transform.
                 * @param position Position to be used.
                 * @return Transformed cartesian coordinate.
                 */
                core::data::environment::Point3 transform(const core::data::environment::Point3 &coordinate, const core::data::environment::Position &position) const;

                /**
                 * This method transforms the given cartesian coordinate from
                 * the world coordinate system to the position coordinate system.
                 *
                 * @param coordinate Coordinate to transform.
                 * @param position Position to be used.
                 * @return Transformed cartesian coordinate.
                 */
                core::data::environment::Point3 transformInversely(const core::data::environment::Point3 &coordinate, const core::data::environment::Position &position) const;
        };

    }
} // hesperia::math

#endif /*HESPERIA_MATH_TRANSFORMATION_H_*/

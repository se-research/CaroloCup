/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/math/Transformation.h"

namespace hesperia {
    namespace math {

        using namespace std;
        using namespace core::data;
        using namespace core::data::environment;

        Transformation::Transformation() {}

        Transformation::~Transformation() {}

        Point3 Transformation::transform(const Point3 &coordinate, const Position &position) const {
            Point3 cc = coordinate;

            // Rotate the coordinate.
            cc.rotateX(position.getRotation().getX());
            cc.rotateY(position.getRotation().getY());
            cc.rotateZ(position.getRotation().getZ());

            // Translate the coordinate.
            cc += position.getPosition();

            return cc;
        }

        Point3 Transformation::transformInversely(const Point3 &coordinate, const Position &position) const {
            Point3 cc = coordinate;

            // Rotate the coordinate.
            cc.rotateX(-1 * position.getRotation().getX());
            cc.rotateY(-1 * position.getRotation().getY());
            cc.rotateZ(-1 * position.getRotation().getZ());

            // Translate the coordinate.
            cc -= position.getPosition();

            return cc;
        }

    }
} // hesperia::math

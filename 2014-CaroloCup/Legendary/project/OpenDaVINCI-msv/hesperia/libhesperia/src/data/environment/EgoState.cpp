/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "hesperia/data/environment/EgoState.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;
            using namespace core::base;
            using namespace core::data::environment;

            EgoState::EgoState() :
                    PointShapedObject() {}

            EgoState::EgoState(const Point3 &position, const Point3 &rotation,
                               const Point3 &velocity, const Point3 &acceleration) :
                    PointShapedObject(position, rotation, velocity, acceleration) {}

            EgoState::EgoState(const EgoState &obj) :
                    PointShapedObject(obj) {}

            EgoState::~EgoState() {}

            EgoState& EgoState::operator=(const EgoState &obj) {
                PointShapedObject::operator=(obj);

                return (*this);
            }

            const string EgoState::toString() const {
                stringstream s;
                s << PointShapedObject::toString();
                return s.str();
            }

            ostream& EgoState::operator<<(ostream &out) const {
                // Serializer super class.
                PointShapedObject::operator<<(out);

                // Serialize this class.
//                SerializationFactory sf;
//
//                Serializer &s = sf.getSerializer(out);

                // Nothing to be done.

                return out;
            }

            istream& EgoState::operator>>(istream &in) {
                // Deserializer super class.
                PointShapedObject::operator>>(in);

                // Deserialize this class.
//                SerializationFactory sf;
//
//                Deserializer &d = sf.getDeserializer(in);

                // Nothing to be done.

                return in;
            }

        }
    }
} // hesperia::data::environment

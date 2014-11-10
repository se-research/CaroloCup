/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/situation/Vertex3.h"

namespace hesperia {
    namespace data {
        namespace situation {

            using namespace std;
            using namespace core::base;
            using namespace core::data::environment;

            Vertex3::Vertex3() :
                    Point3() {}

            Vertex3::Vertex3(const Vertex3 &obj) :
                    Point3(obj) {}

            Vertex3::~Vertex3() {}

            Vertex3& Vertex3::operator=(const Vertex3 &obj) {
                Point3::operator=(obj);

                return (*this);
            }

            void Vertex3::accept(SituationVisitor &visitor) {
                visitor.visit(*this);
            }

        }
    }
} // hesperia::data::situation

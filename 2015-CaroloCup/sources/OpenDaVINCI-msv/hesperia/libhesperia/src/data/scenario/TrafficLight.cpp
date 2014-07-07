/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"
#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "hesperia/data/scenario/TrafficLight.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;
            using namespace scenario;

            TrafficLight::TrafficLight() {
                TrafficControl::setType(TrafficControl::TRAFFICLIGHT);
            }

            TrafficLight::TrafficLight(const TrafficLight &obj) :
                    TrafficControl(obj) {}

            TrafficLight::~TrafficLight() {}

            TrafficLight& TrafficLight::operator=(const TrafficLight &obj) {
                TrafficControl::operator=(obj);

                return (*this);
            }

            void TrafficLight::accept(ScenarioVisitor &visitor) {
                visitor.visit(*this);

                if (getShape() != NULL) {
                    getShape()->accept(visitor);
                }
            }

            const string TrafficLight::toString() const {
                return "Trafficlight";
            }

            ostream& TrafficLight::operator<<(ostream &out) const {
                // Serializer super class.
                TrafficControl::operator<<(out);

                return out;
            }

            istream& TrafficLight::operator>>(istream &in) {
                // Deserializer super class.
                TrafficControl::operator>>(in);

                return in;
            }

        }
    }
} // hesperia::data::scenario

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
#include "hesperia/data/scenario/TrafficSign.h"

namespace hesperia {
    namespace data {
        namespace scenario {

            using namespace std;
            using namespace core::base;
            using namespace scenario;

            TrafficSign::TrafficSign() :
                    m_value("") {
                TrafficControl::setType(TrafficControl::TRAFFICSIGN);
            }

            TrafficSign::TrafficSign(const TrafficSign &obj) :
                    TrafficControl(obj),
                    m_value(obj.getValue()) {}

            TrafficSign::~TrafficSign() {}

            TrafficSign& TrafficSign::operator=(const TrafficSign &obj) {
                TrafficControl::operator=(obj);
                setValue(obj.getValue());

                return (*this);
            }

            void TrafficSign::accept(ScenarioVisitor &visitor) {
                visitor.visit(*this);

                if (getShape() != NULL) {
                    getShape()->accept(visitor);
                }
            }

            const string& TrafficSign::getValue() const {
                return m_value;
            }

            void TrafficSign::setValue(const string &v) {
                m_value = v;
            }

            const string TrafficSign::toString() const {
                stringstream sstr;
                sstr << "Trafficsign: " << getValue();
                return sstr.str();
            }

            ostream& TrafficSign::operator<<(ostream &out) const {
                // Serializer super class.
                TrafficControl::operator<<(out);

                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('v', 'a', 'l', 'u', 'e') >::RESULT,
                        getValue());

                return out;
            }

            istream& TrafficSign::operator>>(istream &in) {
                // Deserializer super class.
                TrafficControl::operator>>(in);

                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('v', 'a', 'l', 'u', 'e') >::RESULT,
                       m_value);

                return in;
            }

        }
    }
} // hesperia::data::scenario

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "hesperia/data/environment/OtherVehicleState.h"

namespace hesperia {
    namespace data {
        namespace environment {

            using namespace std;
            using namespace core::base;
            using namespace core::data::environment;

            OtherVehicleState::OtherVehicleState() :
                    PointShapedObject(),
                    m_id(0) {}

            OtherVehicleState::OtherVehicleState(const uint32_t &id,
						const Point3 &position, const Point3 &rotation,
                        const Point3 &velocity, const Point3 &acceleration) :
                    PointShapedObject(position, rotation, velocity, acceleration),
                    m_id(id) {}

            OtherVehicleState::OtherVehicleState(const OtherVehicleState &obj) :
                    PointShapedObject(obj),
                    m_id(obj.getID()) {}

            OtherVehicleState::~OtherVehicleState() {}

            OtherVehicleState& OtherVehicleState::operator=(const OtherVehicleState &obj) {
                PointShapedObject::operator=(obj);
                setID(obj.getID());

                return (*this);
            }

            const string OtherVehicleState::toString() const {
                stringstream s;
                s << "Other Vehicle State (ID: " << m_id << ")" << PointShapedObject::toString();
                return s.str();
            }

            uint32_t OtherVehicleState::getID() const {
            	return m_id;
            }

            void OtherVehicleState::setID(const uint32_t &id) {
            	m_id = id;
            }

            ostream& OtherVehicleState::operator<<(ostream &out) const {
                // Serializer super class.
                PointShapedObject::operator<<(out);

                // Serialize this class.
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                        m_id);

                return out;
            }

            istream& OtherVehicleState::operator>>(istream &in) {
                // Deserializer super class.
                PointShapedObject::operator>>(in);

                // Deserialize this class.
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'd') >::RESULT,
                       m_id);

                return in;
            }

        }
    }
} // hesperia::data::environment

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "core/data/environment/VehicleData.h"

namespace core {
    namespace data {
        namespace environment {

            using namespace std;
            using namespace core::base;

            VehicleData::VehicleData() :
                m_velocity(),
                m_speed(0),
                m_v_log(0),
                m_v_batt(0),
                m_temp(0),
                m_isSimulation(false) {}

            VehicleData::VehicleData(const VehicleData &obj) :
                m_velocity(obj.m_velocity),
                m_speed(obj.m_speed),
                m_v_log(obj.m_v_log),
                m_v_batt(obj.m_v_batt),
                m_temp(obj.m_temp),
                m_isSimulation(obj.m_isSimulation) {}

            VehicleData::~VehicleData() {}

            VehicleData& VehicleData::operator=(const VehicleData &obj) {
                m_velocity = obj.m_velocity;
                m_speed = obj.m_speed;
                m_v_log = obj.m_v_log;
                m_v_batt = obj.m_v_batt;
                m_temp = obj.m_temp;
                m_isSimulation = obj.m_isSimulation;

                return (*this);
            }

            const Point3 VehicleData::getVelocity() const {
                return m_velocity;
            }

            void VehicleData::setVelocity(const Point3 &velocity) {
                m_velocity = velocity;
            }

            double VehicleData::getSpeed() const {
                return m_speed;
            }

            void VehicleData::setSpeed(const double &speed) {
                m_speed = speed;
            }

            double VehicleData::getV_log() const {
                return m_v_log;
            }

            void VehicleData::setV_log(const double &v_log) {
                m_v_log = v_log;
            }

            double VehicleData::getV_batt() const {
                return m_v_batt;
            }

            void VehicleData::setV_batt(const double &v_batt) {
                m_v_batt = v_batt;
            }

            double VehicleData::getTemp() const {
                return m_temp;
            }

            void VehicleData::setTemp(const double &temp) {
                m_temp = temp;
            }

            bool VehicleData::isSimulation() const {
                return m_isSimulation;
            }

            void VehicleData::setSimulation(const bool &s) {
                m_isSimulation = s;
            }

            const string VehicleData::toString() const {
                stringstream s;
                s << "Velocity: " << m_velocity.toString() << ", speed: " << m_speed << ", V_log: " << m_v_log << ", V_batt: " << m_v_batt << ", temp: " << m_temp << ", simulation = " << m_isSimulation;
                return s.str();
            }

            ostream& VehicleData::operator<<(ostream &out) const {
                // Serialize this class.
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('v', 'e', 'l', 'o', 'c', 'i', 't', 'y') >::RESULT,
                        m_velocity);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('s', 'p', 'e', 'e', 'd') >::RESULT,
                        m_speed);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('v', 'l', 'o', 'g') >::RESULT,
                        m_v_log);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('v', 'b', 'a', 't') >::RESULT,
                        m_v_batt);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'e', 'm', 'p') >::RESULT,
                        m_temp);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('s', 'i', 'm') >::RESULT,
                        m_isSimulation);

                return out;
            }

            istream& VehicleData::operator>>(istream &in) {
                // Deserialize this class.
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('v', 'e', 'l', 'o', 'c', 'i', 't', 'y') >::RESULT,
                       m_velocity);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('s', 'p', 'e', 'e', 'd') >::RESULT,
                       m_speed);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('v', 'l', 'o', 'g') >::RESULT,
                       m_v_log);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('v', 'b', 'a', 't') >::RESULT,
                       m_v_batt);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'e', 'm', 'p') >::RESULT,
                       m_temp);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('s', 'i', 'm') >::RESULT,
                       m_isSimulation);

                return in;
            }

        }
    }
} // core::data::environment

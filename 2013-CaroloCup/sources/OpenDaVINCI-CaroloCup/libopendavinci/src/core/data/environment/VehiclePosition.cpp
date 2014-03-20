/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "core/data/environment/VehiclePosition.h"

namespace core {
    namespace data {
        namespace environment {

            using namespace std;
            using namespace core::base;

            VehiclePosition::VehiclePosition() : Position(),
                m_x(0),
                m_y(0),
                m_heading(0),
                m_traveledPath(0) {}

            VehiclePosition::VehiclePosition(const double &x, const double &y, const double &heading) : Position(),
                m_x(x),
                m_y(y),
                m_heading(heading),
                m_traveledPath(0) {}

            VehiclePosition::VehiclePosition(const VehiclePosition &obj) :
                Position(obj),
                m_x(obj.getX()),
                m_y(obj.getY()),
                m_heading(obj.getHeading()),
                m_traveledPath(obj.getTraveledPath()) {}

            VehiclePosition::~VehiclePosition() {}

            VehiclePosition& VehiclePosition::operator=(const VehiclePosition &obj) {
                Position::operator=(obj);
                setX(obj.getX());
                setY(obj.getY());
                setHeading(obj.getHeading());
                setTraveledPath(obj.getTraveledPath());
                return (*this);
            }

            double VehiclePosition::getX() const {
                return m_x;
            }

            void VehiclePosition::setX(const double &x) {
                m_x = x;
            }

            double VehiclePosition::getY() const {
                return m_y;
            }

            void VehiclePosition::setY(const double &y) {
                m_y = y;
            }

            double VehiclePosition::getHeading() const {
                return m_heading;
            }

            void VehiclePosition::setHeading(const double &heading) {
                m_heading = heading;
            }

            double VehiclePosition::getTraveledPath() const {
                return m_traveledPath;
            }

            void VehiclePosition::setTraveledPath(const double &tp) {
                m_traveledPath = tp;
            }

            const string VehiclePosition::toString() const {
                stringstream s;
                s << Position::toString() << ", x = " << getX() << ", y = " << getY() << ", heading = " << getHeading() << ", traveled path = " << getTraveledPath();
                return s.str();
            }

            ostream& VehiclePosition::operator<<(ostream &out) const {
                // Serializer super class.
                Position::operator<<(out);

                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL1('x') >::RESULT,
                        m_x);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL1('y') >::RESULT,
                        m_y);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('h', 'e', 'a', 'd', 'i', 'n', 'g') >::RESULT,
                        m_heading);

                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('t', 'r', 'a', 'v', 'e', 'l') >::RESULT,
                        m_traveledPath);

                return out;
            }

            istream& VehiclePosition::operator>>(istream &in) {
                // Deserializer super class.
                Position::operator>>(in);

                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL1('x') >::RESULT,
                       m_x);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL1('y') >::RESULT,
                       m_y);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('h', 'e', 'a', 'd', 'i', 'n', 'g') >::RESULT,
                       m_heading);

                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('t', 'r', 'a', 'v', 'e', 'l') >::RESULT,
                       m_traveledPath);

                return in;
            }

        }
    }
} // core::data::environment

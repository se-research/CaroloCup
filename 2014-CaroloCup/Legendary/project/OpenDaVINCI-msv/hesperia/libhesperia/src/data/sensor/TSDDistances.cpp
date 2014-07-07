/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "hesperia/data/sensor/TSDDistances.h"

namespace hesperia {
    namespace data {
        namespace sensor {

            using namespace std;
            using namespace core::base;

            TSDDistances::TSDDistances() :
                SerializableData(), m_distanceMap() {}

            TSDDistances::TSDDistances(const TSDDistances &obj) :
                SerializableData(obj),
                m_distanceMap(obj.m_distanceMap) {}

            TSDDistances::~TSDDistances() {}

            TSDDistances& TSDDistances::operator=(const TSDDistances &obj) {
                setDistanceMap(obj.getDistanceMap());

                return (*this);
            }

            const map<uint32_t, double> TSDDistances::getDistanceMap() const {
                return m_distanceMap;
            }

            void TSDDistances::setDistanceMap(const map<uint32_t, double> &distanceMap) {
                m_distanceMap = distanceMap;
            }

            const string TSDDistances::toString() const {
                stringstream s;
                s << "TSDDistances.";
                return s.str();
            }

            ostream& TSDDistances::operator<<(ostream &out) const {
                // Serialize this class.
                SerializationFactory sf;

                Serializer &s = sf.getSerializer(out);

                // Write contour.
                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('m', 'a', 'p', 's', 'i', 'z', 'e') >::RESULT,
                    static_cast<uint32_t>(m_distanceMap.size()));

                // Key/values.
                stringstream sstr;
                map<uint32_t, double>::const_iterator it = m_distanceMap.begin();
                for (; it != m_distanceMap.end(); it++) {
                    sstr << it->first << endl << it->second << endl;
                }
                s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('d', 'i', 's', 't', 'm', 'a', 'p') >::RESULT,
                    sstr.str());

                return out;
            }

            istream& TSDDistances::operator>>(istream &in) {
                // Deserialize this class.
                SerializationFactory sf;

                Deserializer &d = sf.getDeserializer(in);

                // Read distance map.
                uint32_t numberOfPairs = 0;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('m', 'a', 'p', 's', 'i', 'z', 'e') >::RESULT,
                       numberOfPairs);
                m_distanceMap.clear();

                string serializedDistanceMap;
                d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('d', 'i', 's', 't', 'm', 'a', 'p') >::RESULT,
                       serializedDistanceMap);
                stringstream sstr;
                sstr.str(serializedDistanceMap);
                while (numberOfPairs > 0) {
                    uint32_t id = 0;
                    double dist = -1;

                    sstr >> id >> dist;

                    m_distanceMap[id] = dist;
                    numberOfPairs--;
                }

                return in;
            }

        }
    }
} // hesperia::data::sensor

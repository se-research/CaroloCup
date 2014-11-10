/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "SensorBoardData.h"

namespace msv {

	using namespace std;
	using namespace core::base;

	SensorBoardData::SensorBoardData() : 
        m_sensorDistanceMap()
    {}

	SensorBoardData::SensorBoardData(const SensorBoardData &obj) :
	    SerializableData(),
	    m_sensorDistanceMap(obj.m_sensorDistanceMap)
    {}

	SensorBoardData::~SensorBoardData() {}

	SensorBoardData& SensorBoardData::operator=(const SensorBoardData &obj) {
		m_sensorDistanceMap = obj.m_sensorDistanceMap;

		return (*this);
	}

	uint32_t SensorBoardData::getNumberOfSensors() const {
        return m_sensorDistanceMap.size();
    }

    double SensorBoardData::getDistance(const uint32_t &sensorID) const {
        double distance = 0;

        // Try to find the key/value.
        map<uint32_t, double>::const_iterator it = m_sensorDistanceMap.find(sensorID);
        if (it != m_sensorDistanceMap.end()) {
            distance = it->second;
        }
        else {
            distance = -2;
        }

        return distance;
    }

    void SensorBoardData::update(const uint32_t &sensorID, const double &distance) {
        m_sensorDistanceMap[sensorID] = distance;
    }

	const string SensorBoardData::toString() const {
		stringstream s;

        // Print out distances.
        map<uint32_t, double>::const_iterator it = m_sensorDistanceMap.begin();
        for (; it != m_sensorDistanceMap.end(); it++) {
            s << it->first << "=" << it->second << endl;
        }

		return s.str();
	}

	ostream& SensorBoardData::operator<<(ostream &out) const {
		SerializationFactory sf;

		Serializer &s = sf.getSerializer(out);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'i', 's', 't') >::RESULT,
				toString());

		return out;
	}

	istream& SensorBoardData::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

        string s;
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'i', 's', 't') >::RESULT, s);

		stringstream sstr(s);

        while (!sstr.eof()) {
            string line;
            getline(sstr, line);

            // Trying to find key-value-pair.
            size_t delimiter = line.find_first_of("=");

            // Compute length of value-entry by allowing comments right after values.
            size_t valueLength = line.length();

            // Skip lines with invalid position pointers.
            if (! ( (delimiter > 0) && (valueLength > 0) ) ) {
                continue;
            }

            string key = line.substr(0, delimiter);
            string value = line.substr(delimiter + 1, valueLength);

            // Skip lines with invalid keys or values.
            if ( (key.length() == 0) || (value.length() == 0) ) {
                continue;
            }

            stringstream numKey(key);
            uint32_t _numKey = 0;
            numKey >> _numKey;

            stringstream numValue(value);
            double _numValue = 0;
            numValue >> _numValue;

            update(_numKey, _numValue);
        }

		return in;
	}

} // msv


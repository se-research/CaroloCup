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

#include "SteeringData.h"

namespace msv {

	using namespace std;
	using namespace core::base;

	SteeringData::SteeringData() :
	m_steeringData(0) {}

	SteeringData::SteeringData(const SteeringData &obj) :
			SerializableData(),
			m_steeringData(obj.m_steeringData) {}

	SteeringData::~SteeringData() {}

	SteeringData& SteeringData::operator=(const SteeringData &obj) {
		m_steeringData = obj.m_steeringData;
		return (*this);
	}

	double SteeringData::getSteeringData() const {
		return m_steeringData;
	}

	void SteeringData::setSteeringData(const Lines &lines) {
		m_steeringData = lines;
	}

	const string SteeringData::toString() const {
		stringstream s;
		s << "Example data: " << getExampleData();
		return s.str();
	}

	ostream& SteeringData::operator<<(ostream &out) const {
		SerializationFactory sf;
		Serializer &s = sf.getSerializer(out);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				m_steeringData);
		return out;
	}

	istream& SteeringData::operator>>(istream &in) {
		SerializationFactory sf;
		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				m_steeringData);

		return in;
	}
} //msv

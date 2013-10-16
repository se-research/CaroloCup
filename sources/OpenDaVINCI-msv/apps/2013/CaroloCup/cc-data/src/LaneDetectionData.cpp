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

#include "LaneDetectionData.h"

namespace msv {

	using namespace std;
	using namespace core::base;

	LaneDetectionData::LaneDetectionData() :
	m_lines(0) {}

	LaneDetectionData::LaneDetectionData(const LaneDetectionData &obj) :
			SerializableData(),
			m_lines(obj.m_lines) {}

	LaneDetectionData::~LaneDetectionData() {}

	LaneDetectionData& LaneDetectionData::operator=(const LaneDetectionData &obj) {
		m_lines = obj.m_lines;
		return (*this);
	}

	double LaneDetectionData::getLaneDetectionData() const {
		return m_lines;
	}

	void LaneDetectionData::setLaneDetectionData(const Lines &lines) {
		m_lines = lines;
	}

	const string LaneDetectionData::toString() const {
		stringstream s;
		s << "Example data: " << getExampleData();
		return s.str();
	}

	ostream& LaneDetectionData::operator<<(ostream &out) const {
		SerializationFactory sf;
		Serializer &s = sf.getSerializer(out);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				m_lines);
		return out;
	}

	istream& LaneDetectionData::operator>>(istream &in) {
		SerializationFactory sf;
		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				m_lines);

		return in;
	}
} //msv

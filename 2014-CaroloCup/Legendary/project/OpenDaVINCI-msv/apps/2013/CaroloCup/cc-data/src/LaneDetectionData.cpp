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

namespace carolocup {

	using namespace std;
	using namespace core::base;
	using namespace cv;

	LaneDetectionData::LaneDetectionData() :
          m_lines(Vec4i(0,0,0,0),Vec4i(0,0,0,0),Vec4i(0,0,0,0)) {
       }

	LaneDetectionData::LaneDetectionData(const LaneDetectionData &obj) :
			SerializableData(),
			m_lines(obj.m_lines) {}

	LaneDetectionData::~LaneDetectionData() {}

	LaneDetectionData& LaneDetectionData::operator=(const LaneDetectionData &obj) {
		m_lines = obj.m_lines;
		return (*this);
	}

	Lines LaneDetectionData::getLaneDetectionData() const {
		return m_lines;
	}

	void LaneDetectionData::setLaneDetectionData(const Lines &lines) {
		m_lines = lines;
	}

	const string LaneDetectionData::toString() const {
		stringstream s;

    // Attila 2 Fredrik: what is this 'getSteeringData'? Dont we need some object or something here?
		//s << "Example data: " << getSteeringData().rightLine[0];
		s << "LaneDetectionData";

		return s.str();
	}

	ostream& LaneDetectionData::operator<<(ostream &out) const {
		SerializationFactory sf;
		Serializer &s = sf.getSerializer(out);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				(void*)&m_lines, sizeof(m_lines));
		return out;
	}

	istream& LaneDetectionData::operator>>(istream &in) {
		SerializationFactory sf;
		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				(void*)&m_lines, sizeof(m_lines));

		return in;
	}
} //msv

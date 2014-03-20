/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "LaneDetectionData.h"

namespace carolocup {

	using namespace std;
	using namespace core::base;

	LaneDetectionData::LaneDetectionData() : m_numericalValue(0),
								   m_distance(0) {}

	LaneDetectionData::LaneDetectionData(const LaneDetectionData &obj) :
			SerializableData(),
			m_numericalValue(obj.m_numericalValue),
			m_distance(obj.m_distance) {}

	LaneDetectionData::~LaneDetectionData() {}

	LaneDetectionData& LaneDetectionData::operator=(const LaneDetectionData &obj) {
		m_numericalValue = obj.m_numericalValue;
		m_distance = obj.m_distance;

		return (*this);
	}

	uint32_t LaneDetectionData::getNumericalValue() const {
		return m_numericalValue;
	}

	void LaneDetectionData::setNumericalValue(const uint32_t &nv) {
		m_numericalValue = nv;
	}

	double LaneDetectionData::getDistance() const {
		return m_distance;
	}

	void LaneDetectionData::setDistance(const double &d){
		m_distance = d;
	}

	const string LaneDetectionData::toString() const {
		stringstream s;
		s << m_numericalValue;
		s << " ";
		s << m_distance;
		return s.str();
	}

	ostream& LaneDetectionData::operator<<(ostream &out) const {
		SerializationFactory sf;

		Serializer &s = sf.getSerializer(out);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('n', 'u', 'm') >::RESULT,
				m_numericalValue);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'i', 's','t') >::RESULT,
				m_distance);

		return out;
	}

	istream& LaneDetectionData::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('n', 'u', 'm') >::RESULT,
			   m_numericalValue);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'i', 's','t') >::RESULT,
			   m_distance);

		return in;
	}


} // examples

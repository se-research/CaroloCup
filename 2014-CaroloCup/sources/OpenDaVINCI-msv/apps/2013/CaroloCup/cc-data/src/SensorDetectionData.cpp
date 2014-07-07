/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "SensorDetectionData.h"

namespace carolocup {

	using namespace std;
	using namespace core::base;

	SensorDetectionData::SensorDetectionData() : 
        m_numericalValue(0) {}

	SensorDetectionData::SensorDetectionData(const SensorDetectionData &obj) :
			SerializableData(),
			m_numericalValue(obj.m_numericalValue) {}

	SensorDetectionData::~SensorDetectionData() {}

	SensorDetectionData& SensorDetectionData::operator=(const SensorDetectionData &obj) {
		m_numericalValue = obj.m_numericalValue;

		return (*this);
	}

	double SensorDetectionData::getNumericalValue() const {
		return m_numericalValue;
	}

	void SensorDetectionData::setNumericalValue(const double &nv) {
		m_numericalValue = nv;
	}

	const string SensorDetectionData::toString() const {
		stringstream s;
		s << m_numericalValue;
		return s.str();
	}

	ostream& SensorDetectionData::operator<<(ostream &out) const {
		SerializationFactory sf;

		Serializer &s = sf.getSerializer(out);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'u', 'm','b') >::RESULT,
				m_numericalValue);

		return out;
	}

	istream& SensorDetectionData::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('n', 'u', 'm','b') >::RESULT,
			   m_numericalValue);

		return in;
	}

} // carolocup

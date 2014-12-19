/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "Example7Data.h"

namespace examples {

	using namespace std;
	using namespace core::base;

	Example7Data::Example7Data() : m_numericalValue(0),
								   m_stringValue("") {}

	Example7Data::Example7Data(const Example7Data &obj) :
			SerializableData(),
			m_numericalValue(obj.m_numericalValue),
			m_stringValue(obj.m_stringValue) {}

	Example7Data::~Example7Data() {}

	Example7Data& Example7Data::operator=(const Example7Data &obj) {
		m_numericalValue = obj.m_numericalValue;
		m_stringValue = obj.m_stringValue;

		return (*this);
	}

	uint32_t Example7Data::getNumericalValue() const {
		return m_numericalValue;
	}

	void Example7Data::setNumericalValue(const uint32_t &nv) {
		m_numericalValue = nv;
	}

	const string Example7Data::getStringValue() const {
		return m_stringValue;
	}

	void Example7Data::setStringValue(const string &sv) {
		m_stringValue = sv;
	}

	const string Example7Data::toString() const {
		stringstream s;
		s << m_numericalValue;
		s << " ";
		s << m_stringValue;
		return s.str();
	}

	ostream& Example7Data::operator<<(ostream &out) const {
		SerializationFactory sf;

		Serializer &s = sf.getSerializer(out);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('n', 'u', 'm') >::RESULT,
				m_numericalValue);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('s', 't', 'r') >::RESULT,
				m_stringValue);

		return out;
	}

	istream& Example7Data::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('n', 'u', 'm') >::RESULT,
			   m_numericalValue);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('s', 't', 'r') >::RESULT,
			   m_stringValue);

		return in;
	}


} // examples

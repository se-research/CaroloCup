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

#include "STM32F4Data.h"

namespace msv {

	using namespace std;
	using namespace core::base;

	STM32F4Data::STM32F4Data() : 
        m_rawData()
    {}

	STM32F4Data::STM32F4Data(const STM32F4Data &obj) :
	    SerializableData(),
	    m_rawData(obj.m_rawData)
    {}

	STM32F4Data::~STM32F4Data() {}

	STM32F4Data& STM32F4Data::operator=(const STM32F4Data &obj) {
		m_rawData = obj.m_rawData;

		return (*this);
	}

	string STM32F4Data::getRawData() const {
        return m_rawData;
    }

	void STM32F4Data::setRawData(const string &s) {
        m_rawData = s;
    }

	const string STM32F4Data::toString() const {
		return m_rawData;
	}

	ostream& STM32F4Data::operator<<(ostream &out) const {
		SerializationFactory sf;

		Serializer &s = sf.getSerializer(out);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('r', 'a', 'w') >::RESULT,
				m_rawData);

		return out;
	}

	istream& STM32F4Data::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('r', 'a', 'w') >::RESULT, m_rawData);

		return in;
	}

} // msv


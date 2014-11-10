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

#include "STM32F4Control.h"

namespace msv {

	using namespace std;
	using namespace core::base;

	STM32F4Control::STM32F4Control() : 
        m_dataFeed(0)
    {}

	STM32F4Control::STM32F4Control(const STM32F4Control &obj) :
	    SerializableData(),
	    m_dataFeed(obj.m_dataFeed)
    {}

	STM32F4Control::~STM32F4Control() {}

	STM32F4Control& STM32F4Control::operator=(const STM32F4Control &obj) {
		m_dataFeed = obj.m_dataFeed;

		return (*this);
	}

	uint32_t STM32F4Control::getDataFeed() const {
        return m_dataFeed;
    }

	void STM32F4Control::setDataFeed(const uint32_t &f) {
        m_dataFeed = f;
    }

	const string STM32F4Control::toString() const {
        stringstream sstr;
        sstr << "Feed: " << m_dataFeed;
		return sstr.str();
	}

	ostream& STM32F4Control::operator<<(ostream &out) const {
		SerializationFactory sf;

		Serializer &s = sf.getSerializer(out);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('f', 'e', 'e', 'd') >::RESULT,
				m_dataFeed);

		return out;
	}

	istream& STM32F4Control::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('f', 'e', 'e', 'd') >::RESULT, m_dataFeed);

		return in;
	}

} // msv


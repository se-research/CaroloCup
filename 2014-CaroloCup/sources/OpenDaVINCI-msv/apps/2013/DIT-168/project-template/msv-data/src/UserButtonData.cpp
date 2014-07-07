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

#include "UserButtonData.h"

namespace msv {

	using namespace std;
	using namespace core::base;

	UserButtonData::UserButtonData() : 
        m_buttonStatus(UNAVAILABLE),
        m_duration(0) {}

	UserButtonData::UserButtonData(const UserButtonData &obj) :
	    SerializableData(),
        m_buttonStatus(obj.m_buttonStatus),
        m_duration(obj.m_duration) {}

	UserButtonData::~UserButtonData() {}

	UserButtonData& UserButtonData::operator=(const UserButtonData &obj) {
		m_buttonStatus = obj.m_buttonStatus;
        m_duration = obj.m_duration;

		return (*this);
	}

	UserButtonData::BUTTONSTATUS UserButtonData::getButtonStatus() const {
        return m_buttonStatus;
    }

	void UserButtonData::setButtonStatus(const UserButtonData::BUTTONSTATUS &b) {
        m_buttonStatus = b;
    }

    double UserButtonData::getDuration() const {
        return m_duration;
    }

    void UserButtonData::setDuration(const double &d) {
        m_duration = d;
    }

	const string UserButtonData::toString() const {
		stringstream s;

        s << "Button status: " << getButtonStatus() << ", duration for last pressing: " << getDuration();

		return s.str();
	}

	ostream& UserButtonData::operator<<(ostream &out) const {
		SerializationFactory sf;

		Serializer &s = sf.getSerializer(out);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('s', 't', 'a', 't', 'u', 's') >::RESULT,
				m_buttonStatus);

		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('d', 'u', 'r') >::RESULT,
				m_duration);

		return out;
	}

	istream& UserButtonData::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

        uint32_t status = 0;
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('s', 't', 'a', 't', 'u', 's') >::RESULT,
			  status);
        m_buttonStatus = static_cast<BUTTONSTATUS>(status);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('d', 'u', 'r') >::RESULT,
              m_duration);

		return in;
	}

} // msv


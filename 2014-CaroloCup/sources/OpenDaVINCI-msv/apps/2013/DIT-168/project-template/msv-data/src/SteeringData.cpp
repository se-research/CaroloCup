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
        m_exampleData(0) {}

	SteeringData::SteeringData(const SteeringData &obj) :
	    SerializableData(),
        m_exampleData(obj.m_exampleData) {}

	SteeringData::~SteeringData() {}

	SteeringData& SteeringData::operator=(const SteeringData &obj) {
        m_exampleData = obj.m_exampleData;

		return (*this);
	}

    double SteeringData::getExampleData() const {
        return m_exampleData;
    }

    void SteeringData::setExampleData(const double &e) {
        m_exampleData = e;
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
				m_exampleData);

		return out;
	}

	istream& SteeringData::operator>>(istream &in) {
		SerializationFactory sf;

		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
               m_exampleData);

		return in;
	}

} // msv


/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/STM32F4Data.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	STM32F4Data::STM32F4Data() :
	    SerializableData()
		, m_rawData("")
	{}
	
	STM32F4Data::STM32F4Data(const STM32F4Data &obj) :
	    SerializableData()
		, m_rawData(obj.m_rawData)
	{}
	
	STM32F4Data::~STM32F4Data() {}
	
	STM32F4Data& STM32F4Data::operator=(const STM32F4Data &obj) {
		m_rawData = obj.m_rawData;
		return (*this);
	}
	
	std::string STM32F4Data::getRawData() const {
		return m_rawData;
	}
	
	void STM32F4Data::setRawData(const std::string &val) {
		m_rawData = val;
	}
	
	const string STM32F4Data::toString() const {
		stringstream s;
	
		s << "RawData: " << getRawData() << " ";
	
		return s.str();
	}
	
	ostream& STM32F4Data::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		s.write(CRC32 < CharList<'r', CharList<'a', CharList<'w', CharList<'D', CharList<'a', CharList<'t', CharList<'a', NullType> > > > > > >  >::RESULT,
				m_rawData);
		return out;
	}
	
	istream& STM32F4Data::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		d.read(CRC32 < CharList<'r', CharList<'a', CharList<'w', CharList<'D', CharList<'a', CharList<'t', CharList<'a', NullType> > > > > > >  >::RESULT,
				m_rawData);
		return in;
	}
} // msv

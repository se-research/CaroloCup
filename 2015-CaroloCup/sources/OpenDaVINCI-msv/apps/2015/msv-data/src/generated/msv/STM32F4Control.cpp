/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/STM32F4Control.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	STM32F4Control::STM32F4Control() :
	    SerializableData()
		, m_dataFeed(0)
	{}
	
	STM32F4Control::STM32F4Control(const STM32F4Control &obj) :
	    SerializableData()
		, m_dataFeed(obj.m_dataFeed)
	{}
	
	STM32F4Control::~STM32F4Control() {}
	
	STM32F4Control& STM32F4Control::operator=(const STM32F4Control &obj) {
		m_dataFeed = obj.m_dataFeed;
		return (*this);
	}
	
	uint32_t STM32F4Control::getDataFeed() const {
		return m_dataFeed;
	}
	
	void STM32F4Control::setDataFeed(const uint32_t &val) {
		m_dataFeed = val;
	}
	
	const string STM32F4Control::toString() const {
		stringstream s;
	
		s << "DataFeed: " << getDataFeed() << " ";
	
		return s.str();
	}
	
	ostream& STM32F4Control::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		s.write(CRC32 < CharList<'d', CharList<'a', CharList<'t', CharList<'a', CharList<'F', CharList<'e', CharList<'e', CharList<'d', NullType> > > > > > > >  >::RESULT,
				m_dataFeed);
		return out;
	}
	
	istream& STM32F4Control::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		d.read(CRC32 < CharList<'d', CharList<'a', CharList<'t', CharList<'a', CharList<'F', CharList<'e', CharList<'e', CharList<'d', NullType> > > > > > > >  >::RESULT,
				m_dataFeed);
		return in;
	}
} // msv

/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/CustomLine.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	CustomLine::CustomLine() :
	    SerializableData()
		, p1()
		, p2()
		, slope(0)
		, polygonIndex(-1) // TODO: Validation if the default value is of the desired type.
	{}
	
	CustomLine::CustomLine(const CustomLine &obj) :
	    SerializableData()
		, p1(obj.p1)
		, p2(obj.p2)
		, slope(obj.slope)
		, polygonIndex(obj.polygonIndex)
	{}
	
	CustomLine::~CustomLine() {}
	
	CustomLine& CustomLine::operator=(const CustomLine &obj) {
		p1 = obj.p1;
		p2 = obj.p2;
		slope = obj.slope;
		polygonIndex = obj.polygonIndex;
		return (*this);
	}
	
	MyPoint CustomLine::getP1() const {
		return p1;
	}
	
	void CustomLine::setP1(const MyPoint &val) {
		p1 = val;
	}
	MyPoint CustomLine::getP2() const {
		return p2;
	}
	
	void CustomLine::setP2(const MyPoint &val) {
		p2 = val;
	}
	float CustomLine::getSlope() const {
		return slope;
	}
	
	void CustomLine::setSlope(const float &val) {
		slope = val;
	}
	int32_t CustomLine::getPolygonIndex() const {
		return polygonIndex;
	}
	
	void CustomLine::setPolygonIndex(const int32_t &val) {
		polygonIndex = val;
	}
	
	const string CustomLine::toString() const {
		stringstream s;
	
		s << "P1: " << getP1().toString() << " ";
		s << "P2: " << getP2().toString() << " ";
		s << "Slope: " << getSlope() << " ";
		s << "PolygonIndex: " << getPolygonIndex() << " ";
	
		return s.str();
	}
	
	ostream& CustomLine::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		s.write(CRC32 < CharList<'p', CharList<'1', NullType> >  >::RESULT,
				p1);
		s.write(CRC32 < CharList<'p', CharList<'2', NullType> >  >::RESULT,
				p2);
		s.write(CRC32 < CharList<'s', CharList<'l', CharList<'o', CharList<'p', CharList<'e', NullType> > > > >  >::RESULT,
				slope);
		s.write(CRC32 < CharList<'p', CharList<'o', CharList<'l', CharList<'y', CharList<'g', CharList<'o', CharList<'n', CharList<'I', CharList<'n', CharList<'d', CharList<'e', CharList<'x', NullType> > > > > > > > > > > >  >::RESULT,
				polygonIndex);
		return out;
	}
	
	istream& CustomLine::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		d.read(CRC32 < CharList<'p', CharList<'1', NullType> >  >::RESULT,
				p1);
		d.read(CRC32 < CharList<'p', CharList<'2', NullType> >  >::RESULT,
				p2);
		d.read(CRC32 < CharList<'s', CharList<'l', CharList<'o', CharList<'p', CharList<'e', NullType> > > > >  >::RESULT,
				slope);
		d.read(CRC32 < CharList<'p', CharList<'o', CharList<'l', CharList<'y', CharList<'g', CharList<'o', CharList<'n', CharList<'I', CharList<'n', CharList<'d', CharList<'e', CharList<'x', NullType> > > > > > > > > > > >  >::RESULT,
				polygonIndex);
		return in;
	}
} // msv

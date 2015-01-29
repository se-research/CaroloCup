/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/MyPoint.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	MyPoint::MyPoint() :
	    SerializableData()
		, x(0.0)
		, y(0.0)
	{}
	
	MyPoint::MyPoint(const MyPoint &obj) :
	    SerializableData()
		, x(obj.x)
		, y(obj.y)
	{}
	
	MyPoint::~MyPoint() {}
	
	MyPoint& MyPoint::operator=(const MyPoint &obj) {
		x = obj.x;
		y = obj.y;
		return (*this);
	}
	
	double MyPoint::getX() const {
		return x;
	}
	
	void MyPoint::setX(const double &val) {
		x = val;
	}
	double MyPoint::getY() const {
		return y;
	}
	
	void MyPoint::setY(const double &val) {
		y = val;
	}
	
	const string MyPoint::toString() const {
		stringstream s;
	
		s << "X: " << getX() << " ";
		s << "Y: " << getY() << " ";
	
		return s.str();
	}
	
	ostream& MyPoint::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		s.write(CRC32 < CharList<'x', NullType>  >::RESULT,
				x);
		s.write(CRC32 < CharList<'y', NullType>  >::RESULT,
				y);
		return out;
	}
	
	istream& MyPoint::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		d.read(CRC32 < CharList<'x', NullType>  >::RESULT,
				x);
		d.read(CRC32 < CharList<'y', NullType>  >::RESULT,
				y);
		return in;
	}
} // msv

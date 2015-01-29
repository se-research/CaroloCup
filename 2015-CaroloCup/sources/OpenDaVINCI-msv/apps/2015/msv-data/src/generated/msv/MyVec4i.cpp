/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/MyVec4i.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	MyVec4i::MyVec4i() :
	    SerializableData()
		, x(0)
		, y(0)
		, z(0)
		, w(0)
	{}
	
	MyVec4i::MyVec4i(const MyVec4i &obj) :
	    SerializableData()
		, x(obj.x)
		, y(obj.y)
		, z(obj.z)
		, w(obj.w)
	{}
	
	MyVec4i::~MyVec4i() {}
	
	MyVec4i& MyVec4i::operator=(const MyVec4i &obj) {
		x = obj.x;
		y = obj.y;
		z = obj.z;
		w = obj.w;
		return (*this);
	}
	
	int32_t MyVec4i::getX() const {
		return x;
	}
	
	void MyVec4i::setX(const int32_t &val) {
		x = val;
	}
	int32_t MyVec4i::getY() const {
		return y;
	}
	
	void MyVec4i::setY(const int32_t &val) {
		y = val;
	}
	int32_t MyVec4i::getZ() const {
		return z;
	}
	
	void MyVec4i::setZ(const int32_t &val) {
		z = val;
	}
	int32_t MyVec4i::getW() const {
		return w;
	}
	
	void MyVec4i::setW(const int32_t &val) {
		w = val;
	}
	
	const string MyVec4i::toString() const {
		stringstream s;
	
		s << "X: " << getX() << " ";
		s << "Y: " << getY() << " ";
		s << "Z: " << getZ() << " ";
		s << "W: " << getW() << " ";
	
		return s.str();
	}
	
	int32_t MyVec4i::operator[](int i) const{
		int32_t retVal;		
		switch(i){
			case 0: retVal=getX();break;
			case 1: retVal=getY();break;
			case 2: retVal=getZ();break;
			case 3: retVal=getW();break;
		}
		return retVal;
	}
	
	ostream& MyVec4i::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		s.write(CRC32 < CharList<'x', NullType>  >::RESULT,
				x);
		s.write(CRC32 < CharList<'y', NullType>  >::RESULT,
				y);
		s.write(CRC32 < CharList<'z', NullType>  >::RESULT,
				z);
		s.write(CRC32 < CharList<'w', NullType>  >::RESULT,
				w);
		return out;
	}
	
	istream& MyVec4i::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		d.read(CRC32 < CharList<'x', NullType>  >::RESULT,
				x);
		d.read(CRC32 < CharList<'y', NullType>  >::RESULT,
				y);
		d.read(CRC32 < CharList<'z', NullType>  >::RESULT,
				z);
		d.read(CRC32 < CharList<'w', NullType>  >::RESULT,
				w);
		return in;
	}
} // msv

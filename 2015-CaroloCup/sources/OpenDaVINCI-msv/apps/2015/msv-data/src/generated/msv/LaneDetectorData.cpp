/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/LaneDetectorData.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	LaneDetectorData::LaneDetectorData() :
	    SerializableData()
		, m_frame_count(0)
		, m_lines()
		, m_classification("")
		, m_dataToDriver()
	{}
	
	LaneDetectorData::LaneDetectorData(const LaneDetectorData &obj) :
	    SerializableData()
		, m_frame_count(obj.m_frame_count)
		, m_lines(obj.m_lines)
		, m_classification(obj.m_classification)
		, m_dataToDriver(obj.m_dataToDriver)
	{}
	
	LaneDetectorData::~LaneDetectorData() {}
	
	LaneDetectorData& LaneDetectorData::operator=(const LaneDetectorData &obj) {
		m_frame_count = obj.m_frame_count;
		m_lines = obj.m_lines;
		m_classification = obj.m_classification;
		m_dataToDriver = obj.m_dataToDriver;
		return (*this);
	}
	
	uint32_t LaneDetectorData::getFrameCount() const {
		return m_frame_count;
	}
	
	void LaneDetectorData::setFrameCount(const uint32_t &val) {
		m_frame_count = val;
	}
	Lines LaneDetectorData::getLaneDetectorData() const {
		return m_lines;
	}
	
	void LaneDetectorData::setM_lines(const Lines &val) {
		m_lines = val;
	}
	std::string LaneDetectorData::getClassification() const {
		return m_classification;
	}
	
	void LaneDetectorData::setClassification(const std::string &val) {
		m_classification = val;
	}
	LaneDetectorDataToDriver LaneDetectorData::getLaneDetectorDataDriver() const {
		return m_dataToDriver;
	}
	
	void LaneDetectorData::setM_dataToDriver(const LaneDetectorDataToDriver &val) {
		m_dataToDriver = val;
	}
	
	const string LaneDetectorData::toString() const {
		stringstream s;
	
		s << "M_frame_count: " << getFrameCount() << " ";
		s << "M_lines: " << getLaneDetectorData().toString() << " ";
		s << "M_classification: " << getClassification() << " ";
		s << "M_dataToDriver: " << getLaneDetectorDataDriver().toString() << " ";
	
		return s.str();
	}
	
	ostream& LaneDetectorData::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		s.write(CRC32 < CharList<'m', CharList<'_', CharList<'f', CharList<'r', CharList<'a', CharList<'m', CharList<'e', CharList<'_', CharList<'c', CharList<'o', CharList<'u', CharList<'n', CharList<'t', NullType> > > > > > > > > > > > >  >::RESULT,
				m_frame_count);
		s.write(CRC32 < CharList<'m', CharList<'_', CharList<'l', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > >  >::RESULT,
				m_lines);
		s.write(CRC32 < CharList<'m', CharList<'_', CharList<'c', CharList<'l', CharList<'a', CharList<'s', CharList<'s', CharList<'i', CharList<'f', CharList<'i', CharList<'c', CharList<'a', CharList<'t', CharList<'i', CharList<'o', CharList<'n', NullType> > > > > > > > > > > > > > > >  >::RESULT,
				m_classification);
		s.write(CRC32 < CharList<'m', CharList<'_', CharList<'d', CharList<'a', CharList<'t', CharList<'a', CharList<'T', CharList<'o', CharList<'D', CharList<'r', CharList<'i', CharList<'v', CharList<'e', CharList<'r', NullType> > > > > > > > > > > > > >  >::RESULT,
				m_dataToDriver);
		return out;
	}
	
	istream& LaneDetectorData::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		d.read(CRC32 < CharList<'m', CharList<'_', CharList<'f', CharList<'r', CharList<'a', CharList<'m', CharList<'e', CharList<'_', CharList<'c', CharList<'o', CharList<'u', CharList<'n', CharList<'t', NullType> > > > > > > > > > > > >  >::RESULT,
				m_frame_count);
		d.read(CRC32 < CharList<'m', CharList<'_', CharList<'l', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > >  >::RESULT,
				m_lines);
		d.read(CRC32 < CharList<'m', CharList<'_', CharList<'c', CharList<'l', CharList<'a', CharList<'s', CharList<'s', CharList<'i', CharList<'f', CharList<'i', CharList<'c', CharList<'a', CharList<'t', CharList<'i', CharList<'o', CharList<'n', NullType> > > > > > > > > > > > > > > >  >::RESULT,
				m_classification);
		d.read(CRC32 < CharList<'m', CharList<'_', CharList<'d', CharList<'a', CharList<'t', CharList<'a', CharList<'T', CharList<'o', CharList<'D', CharList<'r', CharList<'i', CharList<'v', CharList<'e', CharList<'r', NullType> > > > > > > > > > > > > >  >::RESULT,
				m_dataToDriver);
		return in;
	}
} // msv

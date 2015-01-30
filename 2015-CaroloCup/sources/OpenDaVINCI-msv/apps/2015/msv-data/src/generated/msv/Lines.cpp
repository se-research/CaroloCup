/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/Lines.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	Lines::Lines() :
	    SerializableData()
		, leftLine()
		, rightLine()
		, dashedLine()
		, goalLine()
		, goalLineLeft()
		, currentLine()
		, pGain(0)
		, intGain(0)
		, derGain(0)
		, speed(0)
		, width(0)
		, height(0)
		, startLineHeight(0)
		, stopLineHeight(0)
		, roadState()
		, confidenceLevel(0)
	{}
	
	Lines::Lines(const Lines &obj) :
	    SerializableData()
		, leftLine(obj.leftLine)
		, rightLine(obj.rightLine)
		, dashedLine(obj.dashedLine)
		, goalLine(obj.goalLine)
		, goalLineLeft(obj.goalLineLeft)
		, currentLine(obj.currentLine)
		, pGain(obj.pGain)
		, intGain(obj.intGain)
		, derGain(obj.derGain)
		, speed(obj.speed)
		, width(obj.width)
		, height(obj.height)
		, startLineHeight(obj.startLineHeight)
		, stopLineHeight(obj.stopLineHeight)
		, roadState(obj.roadState)
		, confidenceLevel(obj.confidenceLevel)
	{}
	
	Lines::~Lines() {}
	
	Lines& Lines::operator=(const Lines &obj) {
		leftLine = obj.leftLine;
		rightLine = obj.rightLine;
		dashedLine = obj.dashedLine;
		goalLine = obj.goalLine;
		goalLineLeft = obj.goalLineLeft;
		currentLine = obj.currentLine;
		pGain = obj.pGain;
		intGain = obj.intGain;
		derGain = obj.derGain;
		speed = obj.speed;
		width = obj.width;
		height = obj.height;
		startLineHeight = obj.startLineHeight;
		stopLineHeight = obj.stopLineHeight;
		roadState = obj.roadState;
		confidenceLevel = obj.confidenceLevel;
		return (*this);
	}
	
	Vec4i Lines::getLeftLine() const {
		return leftLine;
	}
	
	void Lines::setLeftLine(const Vec4i &val) {
		leftLine = val;
	}
	Vec4i Lines::getRightLine() const {
		return rightLine;
	}
	
	void Lines::setRightLine(const Vec4i &val) {
		rightLine = val;
	}
	Vec4i Lines::getDashedLine() const {
		return dashedLine;
	}
	
	void Lines::setDashedLine(const Vec4i &val) {
		dashedLine = val;
	}
	CustomLine Lines::getGoalLine() const {
		return goalLine;
	}
	
	void Lines::setGoalLine(const CustomLine &val) {
		goalLine = val;
	}
	CustomLine Lines::getGoalLineLeft() const {
		return goalLineLeft;
	}
	
	void Lines::setGoalLineLeft(const CustomLine &val) {
		goalLineLeft = val;
	}
	CustomLine Lines::getCurrentLine() const {
		return currentLine;
	}
	
	void Lines::setCurrentLine(const CustomLine &val) {
		currentLine = val;
	}
	int32_t Lines::getPGain() const {
		return pGain;
	}
	
	void Lines::setPGain(const int32_t &val) {
		pGain = val;
	}
	int32_t Lines::getIntGain() const {
		return intGain;
	}
	
	void Lines::setIntGain(const int32_t &val) {
		intGain = val;
	}
	int32_t Lines::getDerGain() const {
		return derGain;
	}
	
	void Lines::setDerGain(const int32_t &val) {
		derGain = val;
	}
	int32_t Lines::getSpeed() const {
		return speed;
	}
	
	void Lines::setSpeed(const int32_t &val) {
		speed = val;
	}
	int32_t Lines::getWidth() const {
		return width;
	}
	
	void Lines::setWidth(const int32_t &val) {
		width = val;
	}
	int32_t Lines::getHeight() const {
		return height;
	}
	
	void Lines::setHeight(const int32_t &val) {
		height = val;
	}
	int32_t Lines::getStartLineHeight() const {
		return startLineHeight;
	}
	
	void Lines::setStartLineHeight(const int32_t &val) {
		startLineHeight = val;
	}
	int32_t Lines::getStopLineHeight() const {
		return stopLineHeight;
	}
	
	void Lines::setStopLineHeight(const int32_t &val) {
		stopLineHeight = val;
	}
	Lines::RoadState Lines::getRoadState() const {
		return roadState;
	}
	
	void Lines::setRoadState(const Lines::RoadState &val) {
		roadState = val;
	}
	int32_t Lines::getConfidenceLevel() const {
		return confidenceLevel;
	}
	
	void Lines::setConfidenceLevel(const int32_t &val) {
		confidenceLevel = val;
	}
	
	const string Lines::toString() const {
		stringstream s;
	
		s << "LeftLine: " << getLeftLine() << " ";
		s << "RightLine: " << getRightLine() << " ";
		s << "DashedLine: " << getDashedLine() << " ";
		s << "GoalLine: " << getGoalLine().toString() << " ";
		s << "GoalLineLeft: " << getGoalLineLeft().toString() << " ";
		s << "CurrentLine: " << getCurrentLine().toString() << " ";
		s << "PGain: " << getPGain() << " ";
		s << "IntGain: " << getIntGain() << " ";
		s << "DerGain: " << getDerGain() << " ";
		s << "Speed: " << getSpeed() << " ";
		s << "Width: " << getWidth() << " ";
		s << "Height: " << getHeight() << " ";
		s << "StartLineHeight: " << getStartLineHeight() << " ";
		s << "StopLineHeight: " << getStopLineHeight() << " ";
		switch(getRoadState()) {
			case NORMAL :
			s << "RoadState: Lines::NORMAL (1) ";
			break;
			case INTERSECTION :
			s << "RoadState: Lines::INTERSECTION (2) ";
			break;
			case NOT_SET :
			s << "RoadState: Lines::NOT_SET (0) ";
			break;
		}
		s << "ConfidenceLevel: " << getConfidenceLevel() << " ";
	
		return s.str();
	}
	
	ostream& Lines::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		s.write(CRC32 < CharList<'l', CharList<'e', CharList<'f', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > >  >::RESULT,
				(void*)&leftLine, sizeof(leftLine));
		s.write(CRC32 < CharList<'r', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > >  >::RESULT,
				(void*)&rightLine, sizeof(rightLine));
		s.write(CRC32 < CharList<'d', CharList<'a', CharList<'s', CharList<'h', CharList<'e', CharList<'d', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > > >  >::RESULT,
				(void*)&dashedLine, sizeof(dashedLine));
		s.write(CRC32 < CharList<'g', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > >  >::RESULT,
				goalLine);
		s.write(CRC32 < CharList<'g', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'L', CharList<'e', CharList<'f', CharList<'t', NullType> > > > > > > > > > > >  >::RESULT,
				goalLineLeft);
		s.write(CRC32 < CharList<'c', CharList<'u', CharList<'r', CharList<'r', CharList<'e', CharList<'n', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > > > >  >::RESULT,
				currentLine);
		s.write(CRC32 < CharList<'p', CharList<'G', CharList<'a', CharList<'i', CharList<'n', NullType> > > > >  >::RESULT,
				pGain);
		s.write(CRC32 < CharList<'i', CharList<'n', CharList<'t', CharList<'G', CharList<'a', CharList<'i', CharList<'n', NullType> > > > > > >  >::RESULT,
				intGain);
		s.write(CRC32 < CharList<'d', CharList<'e', CharList<'r', CharList<'G', CharList<'a', CharList<'i', CharList<'n', NullType> > > > > > >  >::RESULT,
				derGain);
		s.write(CRC32 < CharList<'s', CharList<'p', CharList<'e', CharList<'e', CharList<'d', NullType> > > > >  >::RESULT,
				speed);
		s.write(CRC32 < CharList<'w', CharList<'i', CharList<'d', CharList<'t', CharList<'h', NullType> > > > >  >::RESULT,
				width);
		s.write(CRC32 < CharList<'h', CharList<'e', CharList<'i', CharList<'g', CharList<'h', CharList<'t', NullType> > > > > >  >::RESULT,
				height);
		s.write(CRC32 < CharList<'s', CharList<'t', CharList<'a', CharList<'r', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'H', CharList<'e', CharList<'i', CharList<'g', CharList<'h', CharList<'t', NullType> > > > > > > > > > > > > > >  >::RESULT,
				startLineHeight);
		s.write(CRC32 < CharList<'s', CharList<'t', CharList<'o', CharList<'p', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'H', CharList<'e', CharList<'i', CharList<'g', CharList<'h', CharList<'t', NullType> > > > > > > > > > > > > >  >::RESULT,
				stopLineHeight);
		int32_t int32t_roadState = roadState;
		s.write(CRC32 < CharList<'r', CharList<'o', CharList<'a', CharList<'d', CharList<'S', CharList<'t', CharList<'a', CharList<'t', CharList<'e', NullType> > > > > > > > >  >::RESULT,
				int32t_roadState);
		s.write(CRC32 < CharList<'c', CharList<'o', CharList<'n', CharList<'f', CharList<'i', CharList<'d', CharList<'e', CharList<'n', CharList<'c', CharList<'e', CharList<'L', CharList<'e', CharList<'v', CharList<'e', CharList<'l', NullType> > > > > > > > > > > > > > >  >::RESULT,
				confidenceLevel);
		return out;
	}
	
	istream& Lines::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		d.read(CRC32 < CharList<'l', CharList<'e', CharList<'f', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > >  >::RESULT,
				(void*)&leftLine, sizeof(leftLine));
		d.read(CRC32 < CharList<'r', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > >  >::RESULT,
				(void*)&rightLine, sizeof(rightLine));
		d.read(CRC32 < CharList<'d', CharList<'a', CharList<'s', CharList<'h', CharList<'e', CharList<'d', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > > >  >::RESULT,
				(void*)&dashedLine, sizeof(dashedLine));
		d.read(CRC32 < CharList<'g', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > >  >::RESULT,
				goalLine);
		d.read(CRC32 < CharList<'g', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'L', CharList<'e', CharList<'f', CharList<'t', NullType> > > > > > > > > > > >  >::RESULT,
				goalLineLeft);
		d.read(CRC32 < CharList<'c', CharList<'u', CharList<'r', CharList<'r', CharList<'e', CharList<'n', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > > > >  >::RESULT,
				currentLine);
		d.read(CRC32 < CharList<'p', CharList<'G', CharList<'a', CharList<'i', CharList<'n', NullType> > > > >  >::RESULT,
				pGain);
		d.read(CRC32 < CharList<'i', CharList<'n', CharList<'t', CharList<'G', CharList<'a', CharList<'i', CharList<'n', NullType> > > > > > >  >::RESULT,
				intGain);
		d.read(CRC32 < CharList<'d', CharList<'e', CharList<'r', CharList<'G', CharList<'a', CharList<'i', CharList<'n', NullType> > > > > > >  >::RESULT,
				derGain);
		d.read(CRC32 < CharList<'s', CharList<'p', CharList<'e', CharList<'e', CharList<'d', NullType> > > > >  >::RESULT,
				speed);
		d.read(CRC32 < CharList<'w', CharList<'i', CharList<'d', CharList<'t', CharList<'h', NullType> > > > >  >::RESULT,
				width);
		d.read(CRC32 < CharList<'h', CharList<'e', CharList<'i', CharList<'g', CharList<'h', CharList<'t', NullType> > > > > >  >::RESULT,
				height);
		d.read(CRC32 < CharList<'s', CharList<'t', CharList<'a', CharList<'r', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'H', CharList<'e', CharList<'i', CharList<'g', CharList<'h', CharList<'t', NullType> > > > > > > > > > > > > > >  >::RESULT,
				startLineHeight);
		d.read(CRC32 < CharList<'s', CharList<'t', CharList<'o', CharList<'p', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'H', CharList<'e', CharList<'i', CharList<'g', CharList<'h', CharList<'t', NullType> > > > > > > > > > > > > >  >::RESULT,
				stopLineHeight);
		int32_t int32t_roadState = 0;
		d.read(CRC32 < CharList<'r', CharList<'o', CharList<'a', CharList<'d', CharList<'S', CharList<'t', CharList<'a', CharList<'t', CharList<'e', NullType> > > > > > > > >  >::RESULT,
				int32t_roadState);
		roadState = static_cast<Lines::RoadState>(int32t_roadState);
		d.read(CRC32 < CharList<'c', CharList<'o', CharList<'n', CharList<'f', CharList<'i', CharList<'d', CharList<'e', CharList<'n', CharList<'c', CharList<'e', CharList<'L', CharList<'e', CharList<'v', CharList<'e', CharList<'l', NullType> > > > > > > > > > > > > > >  >::RESULT,
				confidenceLevel);
		return in;
	}
} // msv

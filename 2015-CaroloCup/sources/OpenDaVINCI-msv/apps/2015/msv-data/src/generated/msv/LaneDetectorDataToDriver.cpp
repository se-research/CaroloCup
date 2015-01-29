/*
 * This software is open source. Please see COPYING and AUTHORS for further information.
 *
 * This file is auto-generated. DO NOT CHANGE AS YOUR CHANGES MIGHT BE OVERWRITTEN!
 */

#include <algorithm>
#include <sstream>
#include <utility>

#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "generated/msv/LaneDetectorDataToDriver.h"

namespace msv {
	using namespace std;
	using namespace core::base;
	
	LaneDetectorDataToDriver::LaneDetectorDataToDriver() :
	    SerializableData()
		, listOfSwitchPointsLeftGoalLines()
		, listOfSwitchPointsRightGoalLines()
		, leftGoalLines()
		, rightGoalLines()
		, currentLine()
		, noTrajectory(false)
	{}
	
	LaneDetectorDataToDriver::LaneDetectorDataToDriver(const LaneDetectorDataToDriver &obj) :
	    SerializableData()
		, listOfSwitchPointsLeftGoalLines(obj.listOfSwitchPointsLeftGoalLines)
		, listOfSwitchPointsRightGoalLines(obj.listOfSwitchPointsRightGoalLines)
		, leftGoalLines(obj.leftGoalLines)
		, rightGoalLines(obj.rightGoalLines)
		, currentLine(obj.currentLine)
		, noTrajectory(obj.noTrajectory)
	{}
	
	LaneDetectorDataToDriver::~LaneDetectorDataToDriver() {}
	
	LaneDetectorDataToDriver& LaneDetectorDataToDriver::operator=(const LaneDetectorDataToDriver &obj) {
		listOfSwitchPointsLeftGoalLines = obj.listOfSwitchPointsLeftGoalLines;
		listOfSwitchPointsRightGoalLines = obj.listOfSwitchPointsRightGoalLines;
		leftGoalLines = obj.leftGoalLines;
		rightGoalLines = obj.rightGoalLines;
		currentLine = obj.currentLine;
		noTrajectory = obj.noTrajectory;
		return (*this);
	}
	
	std::vector<int32_t> LaneDetectorDataToDriver::getListOfSwitchPointsLeftGoalLines() const {
		return listOfSwitchPointsLeftGoalLines;
	}
	
	void LaneDetectorDataToDriver::setListOfSwitchPointsLeftGoalLines(const std::vector<int32_t> &val) {
		listOfSwitchPointsLeftGoalLines = val;
	}
	
	void LaneDetectorDataToDriver::clear_ListOfSwitchPointsLeftGoalLines() {
		listOfSwitchPointsLeftGoalLines.clear();
	}
	
	uint32_t LaneDetectorDataToDriver::getSize_ListOfSwitchPointsLeftGoalLines() const {
		return listOfSwitchPointsLeftGoalLines.size();
	}
	
	bool LaneDetectorDataToDriver::isEmpty_ListOfSwitchPointsLeftGoalLines() const {
		return listOfSwitchPointsLeftGoalLines.empty();
	}
	
	void LaneDetectorDataToDriver::addTo_ListOfSwitchPointsLeftGoalLines(const int32_t &val) {
		listOfSwitchPointsLeftGoalLines.push_back(val);
	}
	
	void LaneDetectorDataToDriver::insertTo_ListOfSwitchPointsLeftGoalLines(const int32_t &val) {
		listOfSwitchPointsLeftGoalLines.insert(listOfSwitchPointsLeftGoalLines.begin(), val);
	}
	
	bool LaneDetectorDataToDriver::contains_ListOfSwitchPointsLeftGoalLines(const int32_t &val) const {
		return std::find(listOfSwitchPointsLeftGoalLines.begin(), listOfSwitchPointsLeftGoalLines.end(), val) != listOfSwitchPointsLeftGoalLines.end();
	}
	
	std::pair<std::vector<int32_t>::iterator, std::vector<int32_t>::iterator> LaneDetectorDataToDriver::iteratorPair_ListOfSwitchPointsLeftGoalLines() {
		return std::make_pair(listOfSwitchPointsLeftGoalLines.begin(), listOfSwitchPointsLeftGoalLines.end());
	}
	std::vector<int32_t> LaneDetectorDataToDriver::getListOfSwitchPointsRightGoalLines() const {
		return listOfSwitchPointsRightGoalLines;
	}
	
	void LaneDetectorDataToDriver::setListOfSwitchPointsRightGoalLines(const std::vector<int32_t> &val) {
		listOfSwitchPointsRightGoalLines = val;
	}
	
	void LaneDetectorDataToDriver::clear_ListOfSwitchPointsRightGoalLines() {
		listOfSwitchPointsRightGoalLines.clear();
	}
	
	uint32_t LaneDetectorDataToDriver::getSize_ListOfSwitchPointsRightGoalLines() const {
		return listOfSwitchPointsRightGoalLines.size();
	}
	
	bool LaneDetectorDataToDriver::isEmpty_ListOfSwitchPointsRightGoalLines() const {
		return listOfSwitchPointsRightGoalLines.empty();
	}
	
	void LaneDetectorDataToDriver::addTo_ListOfSwitchPointsRightGoalLines(const int32_t &val) {
		listOfSwitchPointsRightGoalLines.push_back(val);
	}
	
	void LaneDetectorDataToDriver::insertTo_ListOfSwitchPointsRightGoalLines(const int32_t &val) {
		listOfSwitchPointsRightGoalLines.insert(listOfSwitchPointsRightGoalLines.begin(), val);
	}
	
	bool LaneDetectorDataToDriver::contains_ListOfSwitchPointsRightGoalLines(const int32_t &val) const {
		return std::find(listOfSwitchPointsRightGoalLines.begin(), listOfSwitchPointsRightGoalLines.end(), val) != listOfSwitchPointsRightGoalLines.end();
	}
	
	std::pair<std::vector<int32_t>::iterator, std::vector<int32_t>::iterator> LaneDetectorDataToDriver::iteratorPair_ListOfSwitchPointsRightGoalLines() {
		return std::make_pair(listOfSwitchPointsRightGoalLines.begin(), listOfSwitchPointsRightGoalLines.end());
	}
	std::vector<CustomLine> LaneDetectorDataToDriver::getleftGoalLines() const {
		return leftGoalLines;
	}
	
	void LaneDetectorDataToDriver::setleftGoalLines(const std::vector<CustomLine> &val) {
		leftGoalLines = val;
	}
	
	void LaneDetectorDataToDriver::clear_leftGoalLines() {
		leftGoalLines.clear();
	}
	
	uint32_t LaneDetectorDataToDriver::getSize_leftGoalLines() const {
		return leftGoalLines.size();
	}
	
	bool LaneDetectorDataToDriver::isEmpty_leftGoalLines() const {
		return leftGoalLines.empty();
	}
	
	void LaneDetectorDataToDriver::addTo_leftGoalLines(const CustomLine &val) {
		leftGoalLines.push_back(val);
	}
	
	void LaneDetectorDataToDriver::insertTo_leftGoalLines(const CustomLine &val) {
		leftGoalLines.insert(leftGoalLines.begin(), val);
	}
	
	bool LaneDetectorDataToDriver::contains_leftGoalLines(const CustomLine &val) const {
		bool found = false;
		std::vector<CustomLine>::const_iterator it = leftGoalLines.begin();
		while (it != leftGoalLines.end() && !found) {
		    found |= (*it).toString() == val.toString();
		    it++;
		}
		return found;
	}
	
	std::pair<std::vector<CustomLine>::iterator, std::vector<CustomLine>::iterator> LaneDetectorDataToDriver::iteratorPair_leftGoalLines() {
		return std::make_pair(leftGoalLines.begin(), leftGoalLines.end());
	}
	std::vector<CustomLine> LaneDetectorDataToDriver::getrightGoalLines() const {
		return rightGoalLines;
	}
	
	void LaneDetectorDataToDriver::setrightGoalLines(const std::vector<CustomLine> &val) {
		rightGoalLines = val;
	}
	
	void LaneDetectorDataToDriver::clear_rightGoalLines() {
		rightGoalLines.clear();
	}
	
	uint32_t LaneDetectorDataToDriver::getSize_rightGoalLines() const {
		return rightGoalLines.size();
	}
	
	bool LaneDetectorDataToDriver::isEmpty_rightGoalLines() const {
		return rightGoalLines.empty();
	}
	
	void LaneDetectorDataToDriver::addTo_rightGoalLines(const CustomLine &val) {
		rightGoalLines.push_back(val);
	}
	
	void LaneDetectorDataToDriver::insertTo_rightGoalLines(const CustomLine &val) {
		rightGoalLines.insert(rightGoalLines.begin(), val);
	}
	
	bool LaneDetectorDataToDriver::contains_rightGoalLines(const CustomLine &val) const {
		bool found = false;
		std::vector<CustomLine>::const_iterator it = rightGoalLines.begin();
		while (it != rightGoalLines.end() && !found) {
		    found |= (*it).toString() == val.toString();
		    it++;
		}
		return found;
	}
	
	std::pair<std::vector<CustomLine>::iterator, std::vector<CustomLine>::iterator> LaneDetectorDataToDriver::iteratorPair_rightGoalLines() {
		return std::make_pair(rightGoalLines.begin(), rightGoalLines.end());
	}
	CustomLine LaneDetectorDataToDriver::getCurrentLine() const {
		return currentLine;
	}
	
	void LaneDetectorDataToDriver::setCurrentLine(const CustomLine &val) {
		currentLine = val;
	}
	bool LaneDetectorDataToDriver::getNoTrajectory() const {
		return noTrajectory;
	}
	
	void LaneDetectorDataToDriver::setNoTrajectory(const bool &val) {
		noTrajectory = val;
	}
	
	const string LaneDetectorDataToDriver::toString() const {
		stringstream s;
	
		s << "Number of elements in list of SwitchPointsLeftGoalLines: " << getSize_ListOfSwitchPointsLeftGoalLines() << " ";
		s << "Number of elements in list of SwitchPointsRightGoalLines: " << getSize_ListOfSwitchPointsRightGoalLines() << " ";
		s << "Number of elements in list of LeftGoalLines: " << getSize_leftGoalLines() << " ";
		s << "Number of elements in list of RightGoalLines: " << getSize_rightGoalLines() << " ";
		s << "CurrentLine: " << getCurrentLine().toString() << " ";
		s << "NoTrajectory: " << getNoTrajectory() << " ";
	
		return s.str();
	}
	
	ostream& LaneDetectorDataToDriver::operator<<(ostream &out) const {
		SerializationFactory sf;
	
		Serializer &s = sf.getSerializer(out);
	
		// Write number of elements in listOfSwitchPointsLeftGoalLines.
		const uint32_t numberOfSwitchPointsLeftGoalLines = static_cast<uint32_t>(listOfSwitchPointsLeftGoalLines.size());
		s.write(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		        numberOfSwitchPointsLeftGoalLines);
		
		// Write actual elements into a stringstream.
		std::stringstream sstrOfSwitchPointsLeftGoalLines;
		for (uint32_t i = 0; i < numberOfSwitchPointsLeftGoalLines; i++) {
		    sstrOfSwitchPointsLeftGoalLines << listOfSwitchPointsLeftGoalLines.at(i) << endl;
		}
		
		// Write string of elements.
		if (numberOfSwitchPointsLeftGoalLines > 0) {
			s.write(CRC32 < CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
			        sstrOfSwitchPointsLeftGoalLines.str());
		}
		// Write number of elements in listOfSwitchPointsRightGoalLines.
		const uint32_t numberOfSwitchPointsRightGoalLines = static_cast<uint32_t>(listOfSwitchPointsRightGoalLines.size());
		s.write(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		        numberOfSwitchPointsRightGoalLines);
		
		// Write actual elements into a stringstream.
		std::stringstream sstrOfSwitchPointsRightGoalLines;
		for (uint32_t i = 0; i < numberOfSwitchPointsRightGoalLines; i++) {
		    sstrOfSwitchPointsRightGoalLines << listOfSwitchPointsRightGoalLines.at(i) << endl;
		}
		
		// Write string of elements.
		if (numberOfSwitchPointsRightGoalLines > 0) {
			s.write(CRC32 < CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
			        sstrOfSwitchPointsRightGoalLines.str());
		}
		// Write number of elements in leftGoalLines.
		const uint32_t numberOfLeftGoalLines = static_cast<uint32_t>(leftGoalLines.size());
		s.write(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		        numberOfLeftGoalLines);
		
		// Write actual elements into a stringstream.
		std::stringstream sstrOfLeftGoalLines;
		for (uint32_t i = 0; i < numberOfLeftGoalLines; i++) {
		    sstrOfLeftGoalLines << leftGoalLines.at(i);
		}
		
		// Write string of elements.
		if (numberOfLeftGoalLines > 0) {
			s.write(CRC32 < CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > >  >::RESULT,
			        sstrOfLeftGoalLines.str());
		}
		// Write number of elements in rightGoalLines.
		const uint32_t numberOfRightGoalLines = static_cast<uint32_t>(rightGoalLines.size());
		s.write(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		        numberOfRightGoalLines);
		
		// Write actual elements into a stringstream.
		std::stringstream sstrOfRightGoalLines;
		for (uint32_t i = 0; i < numberOfRightGoalLines; i++) {
		    sstrOfRightGoalLines << rightGoalLines.at(i);
		}
		
		// Write string of elements.
		if (numberOfRightGoalLines > 0) {
			s.write(CRC32 < CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > >  >::RESULT,
			        sstrOfRightGoalLines.str());
		}
		s.write(CRC32 < CharList<'c', CharList<'u', CharList<'r', CharList<'r', CharList<'e', CharList<'n', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > > > >  >::RESULT,
				currentLine);
		s.write(CRC32 < CharList<'n', CharList<'o', CharList<'T', CharList<'r', CharList<'a', CharList<'j', CharList<'e', CharList<'c', CharList<'t', CharList<'o', CharList<'r', CharList<'y', NullType> > > > > > > > > > > >  >::RESULT,
				noTrajectory);
		return out;
	}
	
	istream& LaneDetectorDataToDriver::operator>>(istream &in) {
		SerializationFactory sf;
	
		Deserializer &d = sf.getDeserializer(in);
	
		// Clean up the existing list of SwitchPointsLeftGoalLines.
		listOfSwitchPointsLeftGoalLines.clear();
		
		// Read number of elements in listOfSwitchPointsLeftGoalLines.
		uint32_t numberOfSwitchPointsLeftGoalLines = 0;
		d.read(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		       numberOfSwitchPointsLeftGoalLines);
		
		if (numberOfSwitchPointsLeftGoalLines > 0) {
		    // Read string of elements.
		    string elements;
			d.read(CRC32 < CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
				   elements);
		
		    stringstream sstr(elements);
		
		    // Read actual elements from stringstream.
		    for (uint32_t i = 0; i < numberOfSwitchPointsLeftGoalLines; i++) {
		        int32_t element;
		        sstr >> element;
		        listOfSwitchPointsLeftGoalLines.push_back(element);
		    }
		}
		// Clean up the existing list of SwitchPointsRightGoalLines.
		listOfSwitchPointsRightGoalLines.clear();
		
		// Read number of elements in listOfSwitchPointsRightGoalLines.
		uint32_t numberOfSwitchPointsRightGoalLines = 0;
		d.read(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		       numberOfSwitchPointsRightGoalLines);
		
		if (numberOfSwitchPointsRightGoalLines > 0) {
		    // Read string of elements.
		    string elements;
			d.read(CRC32 < CharList<'S', CharList<'w', CharList<'i', CharList<'t', CharList<'c', CharList<'h', CharList<'P', CharList<'o', CharList<'i', CharList<'n', CharList<'t', CharList<'s', CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
				   elements);
		
		    stringstream sstr(elements);
		
		    // Read actual elements from stringstream.
		    for (uint32_t i = 0; i < numberOfSwitchPointsRightGoalLines; i++) {
		        int32_t element;
		        sstr >> element;
		        listOfSwitchPointsRightGoalLines.push_back(element);
		    }
		}
		// Clean up the existing list of LeftGoalLines.
		leftGoalLines.clear();
		
		// Read number of elements in leftGoalLines.
		uint32_t numberOfLeftGoalLines = 0;
		d.read(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		       numberOfLeftGoalLines);
		
		if (numberOfLeftGoalLines > 0) {
		    // Read string of elements.
		    string elements;
			d.read(CRC32 < CharList<'L', CharList<'e', CharList<'f', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > >  >::RESULT,
				   elements);
		
		    stringstream sstr(elements);
		
		    // Read actual elements from stringstream.
		    for (uint32_t i = 0; i < numberOfLeftGoalLines; i++) {
		        CustomLine element;
		        sstr >> element;
		        leftGoalLines.push_back(element);
		    }
		}
		// Clean up the existing list of RightGoalLines.
		rightGoalLines.clear();
		
		// Read number of elements in rightGoalLines.
		uint32_t numberOfRightGoalLines = 0;
		d.read(CRC32 < CharList<'n', CharList<'u', CharList<'m', CharList<'b', CharList<'e', CharList<'r', CharList<'O', CharList<'f', CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > > > > > > > > > >  >::RESULT,
		       numberOfRightGoalLines);
		
		if (numberOfRightGoalLines > 0) {
		    // Read string of elements.
		    string elements;
			d.read(CRC32 < CharList<'R', CharList<'i', CharList<'g', CharList<'h', CharList<'t', CharList<'G', CharList<'o', CharList<'a', CharList<'l', CharList<'L', CharList<'i', CharList<'n', CharList<'e', CharList<'s', NullType> > > > > > > > > > > > > >  >::RESULT,
				   elements);
		
		    stringstream sstr(elements);
		
		    // Read actual elements from stringstream.
		    for (uint32_t i = 0; i < numberOfRightGoalLines; i++) {
		        CustomLine element;
		        sstr >> element;
		        rightGoalLines.push_back(element);
		    }
		}
		d.read(CRC32 < CharList<'c', CharList<'u', CharList<'r', CharList<'r', CharList<'e', CharList<'n', CharList<'t', CharList<'L', CharList<'i', CharList<'n', CharList<'e', NullType> > > > > > > > > > >  >::RESULT,
				currentLine);
		d.read(CRC32 < CharList<'n', CharList<'o', CharList<'T', CharList<'r', CharList<'a', CharList<'j', CharList<'e', CharList<'c', CharList<'t', CharList<'o', CharList<'r', CharList<'y', NullType> > > > > > > > > > > >  >::RESULT,
				noTrajectory);
		return in;
	}
} // msv

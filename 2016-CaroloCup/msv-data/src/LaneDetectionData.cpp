/*
* Mini-Smart-Vehicles.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "opendavinci/odcore/base/Hash.h"
#include "opendavinci/odcore/base/Deserializer.h"
#include "opendavinci/odcore/base/SerializationFactory.h"
#include "opendavinci/odcore/base/Serializer.h"

#include "LaneDetectionData.h"

namespace msv {

	using namespace std;
	using namespace odcore::base;
	using namespace cv;

	LaneDetectionData::LaneDetectionData() :
		m_frame_count(0),m_lines(Vec4i(0,0,0,0),Vec4i(0,0,0,0),Vec4i(0,0,0,0)),m_classification(),m_dataToDriver() {
       }

	LaneDetectionData::LaneDetectionData(const LaneDetectionData &obj) :
			SerializableData(),
			m_frame_count(obj.m_frame_count),
			m_lines(obj.m_lines),
			m_classification(obj.m_classification),
			m_dataToDriver(obj.m_dataToDriver) {}

	LaneDetectionData::~LaneDetectionData() {}

	LaneDetectionData& LaneDetectionData::operator=(const LaneDetectionData &obj) {
		m_lines = obj.m_lines;
		m_frame_count=obj.m_frame_count;
		m_classification=obj.m_classification;
		m_dataToDriver=obj.m_dataToDriver;
		return (*this);
	}

	Lines LaneDetectionData::getLaneDetectionData() const {
		return m_lines;
	}
	LaneDetectorDataToDriver LaneDetectionData::getLaneDetectionDataDriver() const {
			return m_dataToDriver;
		}

	void LaneDetectionData::setLaneDetectionData(const Lines &lines, const LaneDetectorDataToDriver &dataToDriver) {
		m_lines = lines;
		m_dataToDriver = dataToDriver;
	}

	uint32_t LaneDetectionData::getFrameCount() const {
		return	m_frame_count;
	}

	void LaneDetectionData::setFrameCount(const uint32_t &count) {
		m_frame_count=count;
	}

	const string LaneDetectionData::getClassification() const {
		return m_classification;
	}

	void LaneDetectionData::setClassification(const string &classfi) {
			m_classification= classfi;
		}

    int32_t LaneDetectionData::ID() {
        return 1001;
    }

    int32_t LaneDetectionData::getID() const {
        return 1001;
    }

    const string LaneDetectionData::getLongName() const {
        return "LaneDetectionData";
    }

    const string LaneDetectionData::getShortName() const {
        return "LaneDetectionData";
    }

	const string LaneDetectionData::toString() const {
		stringstream s;

		s << getFrameCount() <<";";

		//leftLine.x ; leftLine.y ; leftLine.z ; leftLine.w
		s << m_lines.leftLine[0] << ";" << m_lines.leftLine[1] << ";"<< m_lines.leftLine[2] << ";"<< m_lines.leftLine[3] << ";";

		//rightLine.x ; rightLine.y ; rightLine.z ; rightLine.w
		s << m_lines.rightLine[0] << ";" << m_lines.rightLine[1] << ";"<< m_lines.rightLine[2] << ";"<< m_lines.rightLine[3] << ";";

		//dashedLine.x ; dashedLine.y ; dashedLine.z ; dashedLine.w
		s << m_lines.dashedLine[0] << ";" << m_lines.dashedLine[1] << ";"<< m_lines.dashedLine[2] << ";"<< m_lines.dashedLine[3] << ";";

		// goalLine.p1.x ; goalLine.p1.y ; goalLine.p2.x ; goalLine.p2.y ; goalLine.slope
		s << m_lines.goalLine.p1.x << ";" << m_lines.goalLine.p1.y << ";" << m_lines.goalLine.p2.x << ";" << m_lines.goalLine.p2.y << ";" << m_lines.goalLine.slope << ";";

		//currentLine.p1.x ; currentLine.p1.y ; currentLine.p2.x ; currentLine.p2.y ; currentLine.slope
		s << m_lines.currentLine.p1.x << ";" << m_lines.currentLine.p1.y << ";" << m_lines.currentLine.p2.x << ";" << m_lines.currentLine.p2.y << ";" << m_lines.currentLine.slope << ";";

		// pGain ; intGain ; derGain ; speed ; width ; height ; startLineHeight ; stopLineHeight
		s << m_lines.pGain << ";" << m_lines.intGain << ";" << m_lines.derGain << ";" << m_lines.speed << ";" << m_lines.width << ";" << m_lines.height << ";";

		// startLineHeight ; stopLineHeight
		s << m_lines.startLineHeight << ";" << m_lines.stopLineHeight << ";";

		//classification
		s << getClassification();

		return s.str();
	}

	ostream& LaneDetectionData::operator<<(ostream &out) const {
        SerializationFactory& sf=SerializationFactory::getInstance();
		odcore::SharedPointer<Serializer> s = sf.getSerializer(out);
		s->write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				(void*)&m_lines, sizeof(m_lines));

		s->write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('m', 'f', 'r', 'a', 'm', 'e', 'c') >::RESULT,
						m_frame_count);

		s->write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('c', 'l', 'a', 's', 's', 'i', 'f') >::RESULT,
						m_classification);

		s->write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('d', 'a', 't', 'a', 't', 'o', 'd') >::RESULT,
						(void*)&m_dataToDriver, sizeof(m_dataToDriver));

		return out;
	}

	istream& LaneDetectionData::operator>>(istream &in) {
        SerializationFactory& sf=SerializationFactory::getInstance();
		odcore::SharedPointer<Deserializer> d = sf.getDeserializer(in);

		d->read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('e', 'x', 'a', 'm', 'p', 'l', 'e') >::RESULT,
				(void*)&m_lines, sizeof(m_lines));

		d->read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('m', 'f', 'r', 'a', 'm', 'e', 'c') >::RESULT,
						m_frame_count);

		d->read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('c', 'l', 'a', 's', 's', 'i', 'f') >::RESULT,
						m_classification);

		d->read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL7('d', 'a', 't', 'a', 't', 'o', 'd') >::RESULT,
						(void*)&m_dataToDriver, sizeof(m_dataToDriver));

		return in;
	}
} //msv

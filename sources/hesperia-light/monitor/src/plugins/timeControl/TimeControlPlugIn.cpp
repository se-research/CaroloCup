/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/timeControl/TimeControlPlugIn.h"

#include "core/base/Lock.h"

#include "BufferedFIFOMultiplexer.h"
#include "QtIncludes.h"

namespace plugins {
namespace timeControl {

using namespace std;
using namespace core::base;
using namespace hesperia::base;

TimeControlPlugIn::TimeControlPlugIn(const string &name,
		const core::base::KeyValueConfiguration &kvc,
		hesperia::base::DataStoreManager &dsm, QWidget *prnt) :
	QObject(prnt),
	MasterPlugIn(name, kvc, dsm, prnt),
	m_TimeControlWidget(NULL),
	m_multiplexer() {

	m_multiplexer = new monitor::BufferedFIFOMultiplexer(dsm);
	setMultiplexer(m_multiplexer);
	setupPlugin();
}

TimeControlPlugIn::~TimeControlPlugIn() {
}

QWidget* TimeControlPlugIn::getQWidget() const {
	return m_TimeControlWidget;
}

void TimeControlPlugIn::setupPlugin() {

  qRegisterMetaType<int32_t>("int32_t");
	m_TimeControlWidget = new TimeControlWidget(*this, getParentQWidget());
	connect(m_multiplexer, SIGNAL(bufferFillChanged(const int32_t)), m_TimeControlWidget, SLOT(setValue(const int32_t)));
	connect(m_multiplexer, SIGNAL(positionChanged(const int32_t)),m_TimeControlWidget, SLOT(setPosition(const int32_t)));
	connect(m_multiplexer, SIGNAL(speedChanged(const float)),m_TimeControlWidget, SLOT(setSpeed(const float)));
	connect(m_multiplexer, SIGNAL(recChanged(bool)), m_TimeControlWidget, SLOT(setRecording(bool)));
	//connect(m_TimeControlWidget, SIGNAL(bufferNearlyFull()), m_multiplexer, SLOT(bufferNearlyFull()));
	connect(m_TimeControlWidget, SIGNAL(positionChanged(int32_t)), m_multiplexer, SLOT(setPosition(const int32_t)));

	//connect button-signals
	connect(m_TimeControlWidget, SIGNAL(pause()),m_multiplexer, SLOT(pause()));
	connect(m_TimeControlWidget, SIGNAL(playFwd()),m_multiplexer, SLOT(playFwd()));
	connect(m_TimeControlWidget, SIGNAL(playRev()),m_multiplexer, SLOT(playRev()));
	connect(m_TimeControlWidget, SIGNAL(forward()),m_multiplexer, SLOT(forward()));
	connect(m_TimeControlWidget, SIGNAL(reverse()),m_multiplexer, SLOT(reverse()));
	connect(m_TimeControlWidget, SIGNAL(stepForward()),m_multiplexer, SLOT(stepForward()));
	connect(m_TimeControlWidget, SIGNAL(stepReverse()),m_multiplexer, SLOT(stepReverse()));
	connect(m_TimeControlWidget, SIGNAL(save()),m_multiplexer, SLOT(saveToDisk()));
	connect(m_TimeControlWidget, SIGNAL(load()),m_multiplexer, SLOT(loadFromDisk()));
	connect(m_TimeControlWidget, SIGNAL(recording(bool)),m_multiplexer, SLOT(setRecordingState(bool)));
        connect(m_TimeControlWidget, SIGNAL(minimumChanged(const int32_t)),m_multiplexer , SLOT(setMinimum(const int32_t)));
        connect(m_TimeControlWidget, SIGNAL(maximumChanged(const int32_t)),m_multiplexer , SLOT(setMaximum(const int32_t)));
}

void TimeControlPlugIn::stopPlugin() {
}

}
}

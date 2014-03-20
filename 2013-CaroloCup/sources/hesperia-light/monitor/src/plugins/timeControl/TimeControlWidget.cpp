/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstring>
#include <cmath>

#include "QtIncludes.h"
#include "plugins/timeControl/TimeControlWidget.h"
#include "plugins/timeControl/MediaButtonGroup.h"

#define BUFFERSIZE 10000

namespace plugins {
namespace timeControl {

using namespace std;

TimeControlWidget::TimeControlWidget(const PlugIn &/*plugIn*/, QWidget *prnt) :
	QWidget(prnt),
	m_bufferBar(NULL),
	m_mediaControl(NULL),
	m_statusBar(NULL),
	m_save(NULL),
	m_load(NULL),
        m_playSpeed(1.0f),
	m_speedLabel(NULL),
	m_bufferFillLabel(NULL),
	m_minLabel(NULL),
	m_maxLabel(NULL)
	{


	QGridLayout* timeControlLayout = new QGridLayout(this);
	m_save = new QPushButton(tr("save to Disk"), this);
	m_save->setIcon(QIcon(":/images/save.gif"));
	m_load = new QPushButton(tr("load from Disk"), this);
	m_load->setIcon(QIcon(":/images/open.gif"));
	m_bufferBar = new BufferBar(this);
	timeControlLayout->addWidget(m_bufferBar, 0, 0, 1, 5);
	m_mediaControl = new MediaButtonGroup(true, this);
	timeControlLayout->addWidget(m_mediaControl, 1,1);
	timeControlLayout->addWidget(m_save, 1, 2);
	timeControlLayout->addWidget(m_load, 1, 3);
	timeControlLayout->setColumnStretch(0, 1);
	timeControlLayout->setColumnStretch(4, 1);

        /*
         * Initialize Label to display Speed
         */
        stringstream speeds;
        speeds <<  m_playSpeed;
        speeds << ( (speeds.str() == "1" || speeds.str() == "0") ? ".0x" : "x");
        m_speedLabel = new QLabel(QString(speeds.str().c_str()));

        /*
         * Initialize Label to display Buffer Fill
         */
        stringstream fills;
        fills << "0" << "/" << BUFFERSIZE;
        m_bufferFillLabel = new QLabel(QString(fills.str().c_str()));

        /*
         * Initialize Labels to display min and max
         */
        m_minLabel = new QLabel(QString("<font color='red'>0</font>"));
        m_maxLabel = new QLabel(QString("<font color='green'>0</font>"));


        /*
         * Add QStatusBar as information display
         */
        m_statusBar = new QStatusBar(this);
        m_statusBar->addPermanentWidget(m_speedLabel,0);
        m_statusBar->addPermanentWidget(m_bufferFillLabel,0);
        m_statusBar->addPermanentWidget(m_minLabel,0);
        m_statusBar->addPermanentWidget(m_maxLabel,0);
        timeControlLayout->addWidget(m_statusBar, 2, 0, 1, 5);


	/*
	 * Forward Signals of the media control to widget own Signals
	 */
        connect(m_mediaControl, SIGNAL(playFwd()), this,SIGNAL(playFwd()) );
        connect(m_mediaControl, SIGNAL(playRev()), this, SIGNAL(playRev()));
        connect(m_mediaControl, SIGNAL(pause()), this, SIGNAL(pause()));
        connect(m_mediaControl, SIGNAL(forward()), this, SIGNAL(forward()));
        connect(m_mediaControl, SIGNAL(reverse()), this, SIGNAL(reverse()));
        connect(m_mediaControl, SIGNAL(stepForward()), this, SIGNAL(stepForward()));
        connect(m_mediaControl, SIGNAL(stepReverse()), this, SIGNAL(stepReverse()));
        connect(m_mediaControl, SIGNAL(recording(bool)), this, SIGNAL(recording(bool)));
        connect(m_save, SIGNAL(clicked()), this, SIGNAL(save()));
        connect(m_load, SIGNAL(clicked()), this, SIGNAL(load()));

        /*
         * Forward Signals of the BufferBar to widget own Signals
         */
        connect(m_bufferBar, SIGNAL(positionChanged(const int32_t)), SIGNAL(positionChanged(const int32_t)));
        connect(m_bufferBar, SIGNAL(bufferNearlyFull()), SIGNAL(bufferNearlyFull()));
        connect(m_bufferBar, SIGNAL(minimumChanged(const int32_t)), SIGNAL(minimumChanged(const int32_t)));
        connect(m_bufferBar, SIGNAL(maximumChanged(const int32_t)), SIGNAL(maximumChanged(const int32_t)));

        /*
         * Connect Min and Max Signals
         */
        connect(m_bufferBar, SIGNAL(minimumChanged(const int32_t)), SLOT(setMinimum(const int32_t)));
        connect(m_bufferBar, SIGNAL(maximumChanged(const int32_t)), SLOT(setMaximum(const int32_t)));

}

TimeControlWidget::~TimeControlWidget() {}

void
TimeControlWidget::setValue(const int32_t &value) {
  m_bufferBar->setFillState(value);

    //adjust Label
    stringstream fills;
    fills << value << "/" << BUFFERSIZE;
    m_bufferFillLabel->setText(fills.str().c_str());
}

void
TimeControlWidget::setPosition(const int32_t &position) {
  m_bufferBar->setPosition(position);
}


void
TimeControlWidget::setSpeed(float speed) {
  stringstream speeds;
  speeds << speed;
  speeds << (speeds.str() == "1" ? ".0x" : "x");
  m_speedLabel->setText(speeds.str().c_str());
  m_playSpeed = speed;
}

void
TimeControlWidget::setMinimum(const int32_t &min) {
  stringstream mins;
  mins << "<font color='red'>" << min << "</font>";
  m_minLabel->setText(mins.str().c_str());
}

void
TimeControlWidget::setMaximum(const int32_t &max) {
  stringstream maxs;
  maxs << "<font color='green'>" << max << "</font>";
  m_maxLabel->setText(maxs.str().c_str());
}

void TimeControlWidget::setRecording(bool rec) {
  m_mediaControl->setRecording(rec);
}

}
}


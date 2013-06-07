/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>
#include <cstring>

#include "plugins/timeControl/MediaButtonGroup.h"

namespace plugins {
  namespace timeControl {

    using namespace std;

    MediaButtonGroup::MediaButtonGroup(QWidget* prnt):
      QWidget(prnt),
      m_stepRev(NULL),
      m_rev(NULL),
      m_playRev(NULL),
      m_playFwd(NULL),
      m_pause(NULL),
      m_fwd(NULL),
      m_stepFwd(NULL),
      m_rec(NULL),
      m_playSpeed(1.0f),
      m_recording(false){

        init();
    }

    MediaButtonGroup::MediaButtonGroup(bool rec, QWidget* prnt):
      QWidget(prnt),
      m_stepRev(NULL),
      m_rev(NULL),
      m_playRev(NULL),
      m_playFwd(NULL),
      m_pause(NULL),
      m_fwd(NULL),
      m_stepFwd(NULL),
      m_rec(NULL),
      m_playSpeed(1.0f),
      m_recording(rec){

        init();
    }

    void
    MediaButtonGroup::init() {
      //Grid Layout for button arrangement
      QGridLayout* buttonLayout = new QGridLayout(this);

      /*
       * - Construct button
       * - set icon
       * - set it checkable (playRev, playFwd, pause, rec)
       * - add it to the layout
       */
      m_stepRev = new QPushButton(this);
      m_stepRev->setIcon(QIcon(":/images/stepRev.png"));
      buttonLayout->addWidget(m_stepRev,0,0);
      m_rev = new QPushButton(this);
      m_rev->setIcon(QIcon(":/images/rev.png"));
      buttonLayout->addWidget(m_rev,0,1);
      m_playRev = new QPushButton(this);
      m_playRev->setIcon(QIcon(":/images/playRev.png"));
      m_playRev->setCheckable(true);
      buttonLayout->addWidget(m_playRev,0,2);
      m_playFwd = new QPushButton(this);
      m_playFwd->setIcon(QIcon(":/images/play.png"));
      m_playFwd->setCheckable(true);
      buttonLayout->addWidget(m_playFwd,0,3);
      m_pause = new QPushButton(this);
      m_pause->setIcon(QIcon(":/images/pause.png"));
      m_pause->setCheckable(true);
      buttonLayout->addWidget(m_pause,0,4);
      m_fwd = new QPushButton(this);
      m_fwd->setIcon(QIcon(":/images/fwd.png"));
      buttonLayout->addWidget(m_fwd,0,5);
      m_stepFwd = new QPushButton(this);
      m_stepFwd->setIcon(QIcon(":/images/stepFwd.png"));
      buttonLayout->addWidget(m_stepFwd,0,6);
      m_rec = new QPushButton(this);
      m_rec->setIcon(QIcon(":/images/rec.png"));
      m_rec->setCheckable(true);
      m_rec->setChecked(m_recording);
      buttonLayout->addWidget(m_rec,0,7);


      /*
       * Construct ButtonGroup to avoid coevally pressed buttons
       */
      QButtonGroup* playGroup = new QButtonGroup(this);
      playGroup->addButton(m_stepRev);
      playGroup->addButton(m_stepFwd);
      playGroup->addButton(m_pause);
      playGroup->addButton(m_playFwd);
      playGroup->addButton(m_playRev);

      /*
       * Connect button signals to corresponding widget signals to
       * indicate a pressed button
       */
      connect(m_playFwd, SIGNAL(clicked()), this, SIGNAL(playFwd()));
      connect(m_playRev, SIGNAL(clicked()), this, SIGNAL(playRev()));
      connect(m_pause, SIGNAL(clicked()), this, SIGNAL(pause()));
      connect(m_fwd, SIGNAL(clicked()), this, SIGNAL(forward()));
      connect(m_rev, SIGNAL(clicked()), this, SIGNAL(reverse()));
      connect(m_stepFwd, SIGNAL(clicked()), this, SIGNAL(stepForward()));
      connect(m_stepRev, SIGNAL(clicked()), this, SIGNAL(stepReverse()));
      connect(m_rec, SIGNAL(toggled(bool)), this, SIGNAL(recording(bool)));
    }

    MediaButtonGroup::~MediaButtonGroup(){}

    void
    MediaButtonGroup::setRecording(bool state) {
      m_rec->setChecked(state);
    }
  }
}

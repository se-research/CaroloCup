/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MEDIABUTTONGROUP_H_
#define MEDIABUTTONGROUP_H_

#include "QtIncludes.h"

namespace plugins {
  namespace timeControl {

    /*
     * This class represents a control interface for media and media-like
     * applications. It provides buttons to control the most common media
     * operations, e.g. play, pause, fast forward...
     * Additionally theres a toggleable record-button which indicates if
     * data is recorded in any way.
     *
     */
    class MediaButtonGroup : public QWidget {

      Q_OBJECT

    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       *
       * @param obj Reference to an object of this class.
       */
      MediaButtonGroup(const MediaButtonGroup &/*obj*/ );

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       *
       * @param obj Reference to an object of this class.
       * @return Reference to this instance.
       */
      MediaButtonGroup&
      operator=(const MediaButtonGroup &/*obj*/);

    public:

      /*
       * The basic constructor of the class.
       *
       * @parm prnt Parent widget. If not given the parent will be null.
       */
      MediaButtonGroup(QWidget* prnt = 0);

      /*
       * Enhanced Constructor.
       * It allowes to control the initial state of the widget.
       *
       * @param prnt Parent widget.
       * @param recording Sets recording on of off. Default is false.
       */
      MediaButtonGroup(bool recording, QWidget* prnt = 0 );

      ~MediaButtonGroup();


    /*
     * Signals are emitted if the respective buttons are are pressed.
     */
    signals:
      void
      playFwd();
      void
      playRev();
      void
      pause();
      void
      forward();
      void
      reverse();
      void
      stepForward();
      void
      stepReverse();
      /*
       * Since Recording can be on or off m_rec is a toggle button
       * and the corresponding signal additionally returns the actual state of recording.
       */
      void
      recording(bool recState);

      /*
       * Slots give control over the member variables and can connected to signals.
       */
    public slots:
      void
      setRecording(bool);

    private:
      //control buttons in right order
      QPushButton *m_stepRev;
      QPushButton *m_rev;
      QPushButton *m_playRev;
      QPushButton *m_playFwd;
      QPushButton *m_pause;
      QPushButton *m_fwd;
      QPushButton *m_stepFwd;
      QPushButton *m_rec;

      //At which speed data is played
      float m_playSpeed;

      //Is recording enabled
      bool m_recording;

      void
      init();
    };
  }
}

#endif /* MEDIABUTTONGROUP_H_ */

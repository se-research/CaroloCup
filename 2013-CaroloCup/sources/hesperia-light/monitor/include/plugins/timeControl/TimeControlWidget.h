/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef TIMECONTROLWIDGET_H_
#define TIMECONTROLWIDGET_H_

#include "QtIncludes.h"

#include "plugins/PlugIn.h"
#include "MediaButtonGroup.h"
#include "BufferBar.h"

namespace plugins
{
  namespace timeControl
  {

    using namespace std;

    /*
     * This class provides widget elements to control the
     * TimeControlPlugin
     */
    class TimeControlWidget : public QWidget
    {

    Q_OBJECT

    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       *
       * @param obj Reference to an object of this class.
       */
      TimeControlWidget(const TimeControlWidget &/*obj*/);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       *
       * @param obj Reference to an object of this class.
       * @return Reference to this instance.
       */
      TimeControlWidget&
      operator=(const TimeControlWidget &/*obj*/);

    public:
      /*
       * The Contructor
       *
       * @param plugIn The plugin the widget belongs to.
       * @param prnt The parent widget.
       */
      TimeControlWidget(const PlugIn &plugIn, QWidget *prnt);

      /*
       * Destructor.
       */
      virtual
      ~TimeControlWidget();


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

      void
      save();
      void
      load();

      /*
       * Forwarded Signals from the BufferBar
       */
      void
      positionChanged(const int32_t &value);
      void
      bufferNearlyFull();
      void minimumChanged(const int32_t &min);
      void maximumChanged(const int32_t &max);

    public slots:
      void
      setValue(const int32_t &value);
      void
      setPosition(const int32_t &value);
      void
      setSpeed(float speed);
      void
      setRecording(bool rec);
      void
      setMinimum(const int32_t &min);
      void
      setMaximum(const int32_t &max);

    private:

      BufferBar* m_bufferBar;

      MediaButtonGroup* m_mediaControl;
      QStatusBar* m_statusBar;

      QPushButton *m_save;
      QPushButton *m_load;

      float m_playSpeed;
      QLabel *m_speedLabel;
      QLabel *m_bufferFillLabel;
      QLabel *m_minLabel;
      QLabel *m_maxLabel;

      void
      showPositionTip();


    };

  }
}

#endif /* TIMECONTROLWIDGET_H_ */

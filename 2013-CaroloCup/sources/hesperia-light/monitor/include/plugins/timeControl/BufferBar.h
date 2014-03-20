/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef BUFFERBAR_H_
#define BUFFERBAR_H_

#include "QtIncludes.h"
#include "Slider.h"
#include "BufferOverlay.h"

namespace plugins
{
  namespace timeControl
  {

    using namespace std;

    class BufferBar : public QWidget
    {

      Q_OBJECT

    private:
      /**
       * "Forbidden" copy constructor. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the copy constructor.
       */
      BufferBar(const BufferBar &/*obj*/);

      /**
       * "Forbidden" assignment operator. Goal: The compiler should warn
       * already at compile time for unwanted bugs caused by any misuse
       * of the assignment operator.
       */
      BufferBar&
      operator=(const BufferBar &/*obj*/);

    public:
      /*
       * Constructor
       *
       * @parm prnt Parent Widget
       */
      BufferBar(QWidget* prnt = 0);

      /*
       * Destructor
       */
      virtual
      ~BufferBar();

    signals:
      void
      positionChanged(const int32_t &value);
      void
      bufferNearlyFull();
      void minimumChanged(const int32_t &min);
      void maximumChanged(const int32_t &max);

    public slots:
      void
      setPosition(const int32_t &position);
      void
      setFillState(const int32_t &fill);
      void
      showPosition(const int32_t &pos_x);

    protected:
      /*
       * Capture Events
       *
       * @parm e Triggered event
       */
      void
      mouseDoubleClickEvent(QMouseEvent *e);

    private:
      QProgressBar *m_bufferFill;
      Slider *m_bufferPosition;

      BufferOverlay *m_bufferOverlay;

    private slots:
      void
      checkSlider(const int &value);
      void
      checkMaximum(const int32_t &max);
      void
      transformAndEmitMax(const int32_t &max);
      void
      transformAndEmitMin(const int32_t &min);
    };

  } // namespace timeControl
} // namespace plugins


#endif /* BUFFERBAR_H_ */

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/timeControl/BufferBar.h"
#include "plugins/timeControl/Slider.h"
#include <iostream>
#include <cmath>
#include <cstring>
#include <sstream>

#define BUFFERSIZE 10000

namespace plugins
{

  namespace timeControl
  {

    using namespace std;

    BufferBar::BufferBar(QWidget* prnt) :
      QWidget(prnt),
      m_bufferFill(NULL), m_bufferPosition(NULL),
      m_bufferOverlay(NULL)
    {

      QGridLayout* BufferBarLayout = new QGridLayout(this);
      BufferBarLayout->setSpacing(0);
      BufferBarLayout->setMargin(0);
      m_bufferFill = new QProgressBar(this);
      m_bufferFill->setTextVisible(false);
      m_bufferFill->setRange(0, BUFFERSIZE);
      m_bufferFill->setValue(0);
      m_bufferFill->setMouseTracking(true);
      m_bufferPosition = new Slider(Qt::Horizontal, this);
      m_bufferPosition->setTickPosition(QSlider::NoTicks);
      m_bufferPosition->setRange(0, BUFFERSIZE);
      m_bufferPosition->setMouseTracking(true);
      BufferBarLayout->addWidget(m_bufferFill,0,0);
      BufferBarLayout->addWidget(m_bufferPosition,0,0);
      m_bufferOverlay = new BufferOverlay(this);
      BufferBarLayout->addWidget(m_bufferOverlay,0,0);

      connect(m_bufferOverlay, SIGNAL(cursorPositionChanged(const int32_t)),SLOT(showPosition(const int32_t)));
      connect(m_bufferPosition, SIGNAL(sliderMoved(int)), SLOT(checkSlider(int)));

      /*
       * Forward BorderSelections
       */
      connect(m_bufferOverlay,SIGNAL(minimumChanged(const int32_t)), SLOT(transformAndEmitMin(const int32_t)));
      connect(m_bufferOverlay,SIGNAL(maximumChanged(const int32_t)), SLOT(transformAndEmitMax(const int32_t)));
      connect(m_bufferOverlay, SIGNAL(maximumChanged(const int32_t)), SLOT(checkMaximum(const int32_t)));

      setMouseTracking(true);
    }

    BufferBar::~BufferBar(){};

    void
    BufferBar::checkSlider(const int &value) {
            if (value> m_bufferFill->value()) {
                    m_bufferPosition->setValue(m_bufferFill->value());
            }
            emit positionChanged(m_bufferPosition->value());
    }

    void
    BufferBar::checkMaximum(const int32_t & max){
      if (max > m_bufferFill->value() * m_bufferFill->width()/BUFFERSIZE) {
        int32_t newMax = m_bufferFill->value() * m_bufferFill->width()/BUFFERSIZE;
        m_bufferOverlay->setMaximum(newMax);
        emit transformAndEmitMax(newMax);
      }
    }

    void BufferBar::setPosition(const int32_t &position){
      m_bufferPosition->setValue(position);
      checkSlider(m_bufferPosition->value());
    }

    void BufferBar::setFillState(const int32_t &fill) {
      m_bufferFill->setValue(fill);
      checkSlider(m_bufferFill->value());
      m_bufferFill->update();

      if (m_bufferFill->value() == (static_cast<int32_t>( 3 * m_bufferFill->maximum() / 4))) {
              emit bufferNearlyFull();
      }
    }

    void BufferBar::mouseDoubleClickEvent(QMouseEvent *e) {
      if (e->button() == Qt::LeftButton) {
        int32_t position =(e->pos().x()) * BUFFERSIZE / m_bufferFill->width();
        setPosition(position);
        //std::cout << "Left klick@: "<< position << std::endl;
      }
    }

    void BufferBar::showPosition(const int32_t &pos_x) {
      if (pos_x <= width() && pos_x >= 0) {
      QToolTip::hideText();
      stringstream posString;
      int32_t position = pos_x * BUFFERSIZE / m_bufferFill->width();
      posString << position;
      QToolTip::showText(mapToGlobal(QPoint(pos_x-15,pos().y()-45)), QString(posString.str().c_str()), this, QRect(0,0,1,1));
      } else {
        QToolTip::hideText();
      }
    }

    void BufferBar::transformAndEmitMin(const int32_t &min) {
      int32_t minAsBufferFillValue = min * BUFFERSIZE / m_bufferFill->width();
      emit minimumChanged(minAsBufferFillValue);
    }

    void BufferBar::transformAndEmitMax(const int32_t &max) {
      int32_t maxAsBufferFillValue = max * BUFFERSIZE / m_bufferFill->width();
      emit maximumChanged(maxAsBufferFillValue);
    }

  } // namespace timeControl

} // namespace plugins

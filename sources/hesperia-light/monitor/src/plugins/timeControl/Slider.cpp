/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/timeControl/Slider.h"
#include "QtIncludes.h"
#include <iostream>

namespace plugins
{

  namespace timeControl
  {

    using namespace std;

    Slider::Slider(Qt::Orientation direction, QWidget* prnt) :
      QSlider(direction, prnt)
    {
    }

    Slider::~Slider()
    {
    }

    void
    Slider::mouseDoubleClickEvent(QMouseEvent* e)
    {
      emit leftButtonClicked(e->pos());
      int32_t newPos = e->pos().x() * maximum() / width();
      setValue(newPos);
      emit sliderMoved(newPos);
    }

  } // namespace timeControl

} // namespace plugins

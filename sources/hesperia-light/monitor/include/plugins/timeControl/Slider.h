/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef SLIDER_H_
#define SLIDER_H_

#include "QtIncludes.h"

namespace plugins {

namespace timeControl {

  class Slider : public QSlider {

    Q_OBJECT

  private:
    /**
     * "Forbidden" copy constructor. Goal: The compiler should warn
     * already at compile time for unwanted bugs caused by any misuse
     * of the copy constructor.
     */
    Slider(const Slider &/*obj*/);

    /**
     * "Forbidden" assignment operator. Goal: The compiler should warn
     * already at compile time for unwanted bugs caused by any misuse
     * of the assignment operator.
     */
    Slider&
    operator=(const Slider &/*obj*/);

  public:
    /*
     * Constructor
     */
    Slider(Qt::Orientation direction, QWidget* prnt=0);

    virtual
    ~Slider();

  signals:
    /*
     * Mouse Button clicked
     *
     * @parm position Position of click relative to the widget
     */
    void leftButtonClicked(const QPoint & pos);

  protected:
    /*
     * Process Mouse Click and distribute
     */
    virtual void mouseDoubleClickEvent( QMouseEvent * e );

  };

}  // namespace timeControl

}  // namespace plugins


#endif /* SLIDER_H_ */

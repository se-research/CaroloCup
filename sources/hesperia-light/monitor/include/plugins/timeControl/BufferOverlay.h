/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef BUFFEROVERLAY_H_
#define BUFFEROVERLAY_H_

#include "QtIncludes.h"

namespace plugins {

namespace timeControl {

class BufferOverlay : public QWidget {

  Q_OBJECT

private:
  /**
   * "Forbidden" copy constructor. Goal: The compiler should warn
   * already at compile time for unwanted bugs caused by any misuse
   * of the copy constructor.
   */
  BufferOverlay(const BufferOverlay &/*obj*/);

  /**
   * "Forbidden" assignment operator. Goal: The compiler should warn
   * already at compile time for unwanted bugs caused by any misuse
   * of the assignment operator.
   */
  BufferOverlay&
  operator=(const BufferOverlay &/*obj*/);

public:
  BufferOverlay(QWidget* prnt=0);
  ~BufferOverlay();

signals:
  void cursorPositionChanged(const int32_t &pos_x);
  void minimumChanged(const int32_t &min);
  void maximumChanged(const int32_t &max);

public slots:
  /*
   * Set Borders of Selection
   */
  void setMinimum(const int32_t &min);
  void setMaximum(const int32_t &max);

protected:
  virtual void paintEvent(QPaintEvent *event);
  virtual void mouseMoveEvent(QMouseEvent* event);
  virtual void mousePressEvent(QMouseEvent * event);
  virtual void mouseReleaseEvent(QMouseEvent * event);
  virtual void keyPressEvent ( QKeyEvent * event );
  virtual void keyReleaseEvent ( QKeyEvent * event );

private:
  QPoint m_leftBorder;
  QPoint m_rightBorder;

  bool m_dragLeftAction;
  bool m_dragRightAction;
  bool m_dragKeyDown;

  void drawMarker(const QPoint &markerPosition, QPainter * pntr);

};

}  // namespace timeControl

}  // namespace plugins

#endif /* BUFFEROVERLAY_H_ */

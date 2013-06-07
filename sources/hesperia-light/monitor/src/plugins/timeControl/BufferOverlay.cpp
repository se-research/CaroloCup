/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/timeControl/BufferOverlay.h"
#include <iostream>

namespace plugins
{

  namespace timeControl
  {

    using namespace std;

    BufferOverlay::BufferOverlay(QWidget* prnt) :
      QWidget(prnt),
      m_leftBorder(0,0), m_rightBorder(0,0),
      m_dragLeftAction(false),
      m_dragRightAction(false),
      m_dragKeyDown(false)
    {
      setFocusPolicy(Qt::StrongFocus);
      setMouseTracking(true);
    }

    BufferOverlay::~BufferOverlay(){}

    void
    BufferOverlay::setMinimum(const int32_t &min)
    {
      m_leftBorder = QPoint(min, 0);
    }

    void
    BufferOverlay::setMaximum(const int32_t &max)
    {
      m_rightBorder = QPoint(max, 0);
    }

    void
    BufferOverlay::paintEvent(QPaintEvent*)
    {
      QPainter* pntr = new QPainter(this);
      pntr->setPen(Qt::red);
      pntr->setBrush(QBrush(Qt::red, Qt::SolidPattern));
      drawMarker(m_leftBorder, pntr);

      pntr->setPen(Qt::green);
      pntr->setBrush(QBrush(Qt::green, Qt::SolidPattern));
      drawMarker(m_rightBorder, pntr);
      delete pntr;
    }

    void
    BufferOverlay::mouseMoveEvent(QMouseEvent* evnt)
    {
      //emit Cursor Position
      emit cursorPositionChanged(evnt->pos().x());

      if ((evnt->pos().x() < m_leftBorder.x() +2 &&
              evnt->pos().x() > m_leftBorder.x() -2)  ||
          (evnt->pos().x() < m_rightBorder.x() +2 &&
              evnt->pos().x() > m_rightBorder.x() -2))
        {
          setCursor(Qt::SizeHorCursor);
        }
      else
        {
          setCursor(Qt::ArrowCursor);
        }

      if (m_dragLeftAction &&
          evnt->pos().x() < m_rightBorder.x() &&
          evnt->pos().x() >= 0)
        {
          m_leftBorder.setX(evnt->pos().x());
          emit minimumChanged(evnt->pos().x());
        }
      if (m_dragRightAction &&
          evnt->pos().x() > m_leftBorder.x() &&
          evnt->pos().x() <= width())
        {
          m_rightBorder.setX(evnt->pos().x());
          emit maximumChanged(evnt->pos().x());
        }
      update();
    }

    void
    BufferOverlay::mousePressEvent(QMouseEvent* e)
    {
      if (e->button() == Qt::LeftButton &&
          m_dragKeyDown &&
          (e->pos().x() < m_leftBorder.x() +2 &&
              e->pos().x() > m_leftBorder.x() -2)  &&
          !m_dragRightAction)
        {
          m_dragLeftAction = true;
        }
      if (e->button() == Qt::RightButton &&
          m_dragKeyDown &&
          (e->pos().x() < m_rightBorder.x() +2 &&
             e->pos().x() > m_rightBorder.x() -2) &&
          !m_dragLeftAction)
        {
          m_dragRightAction = true;
        }
    }
    void
    BufferOverlay::mouseReleaseEvent(QMouseEvent* e)
    {
      if (e->button() == Qt::LeftButton)
        {
          m_dragLeftAction = false;
        }
      if (e->button() == Qt::RightButton)
        {
          m_dragRightAction = false;
        }
    }

    void
    BufferOverlay::keyPressEvent(QKeyEvent * e)
    {
      if (e->key() == Qt::Key_Control)
        {
          m_dragKeyDown = true;
        }
    }

    void
    BufferOverlay::keyReleaseEvent(QKeyEvent * e)
    {
      if (e->key() == Qt::Key_Control)
        {
          m_dragKeyDown = false;
          m_dragLeftAction = false;
          m_dragRightAction = false;
        }
    }

    void
    BufferOverlay::drawMarker(const QPoint &markerPosition, QPainter *pntr)
    {

      int32_t x_coord = markerPosition.x();

      QPoint borderLine[8] =
        { QPoint(x_coord - 3, 0), QPoint(x_coord + 3, 0), QPoint(x_coord, 3),
            QPoint(x_coord, height() - 4), QPoint(x_coord + 3, height() - 1),
            QPoint(x_coord - 3, height() - 1), QPoint(x_coord, height() - 4),
            QPoint(x_coord, 3) };

      pntr->drawPolygon(borderLine, 8, Qt::WindingFill);
    }

  } // namespace timeControl

} // namespace plugins

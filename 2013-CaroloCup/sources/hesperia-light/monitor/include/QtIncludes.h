/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */


#ifndef MONITOR_QTINCLUDES_H_
#define MONITOR_QTINCLUDES_H_

#if defined __GNUC__
#pragma GCC system_header
#elif defined __SUNPRO_CC
#pragma disable_warn
#elif defined _MSC_VER
#pragma warning(push, 1)
#endif

#include <Qt/qaction.h>
#include <Qt/qapplication.h>
#include <Qt/qcombobox.h>
#include <Qt/qevent.h>
#include <Qt/qfile.h>
#include <Qt/qfiledialog.h>
#include <Qt/qfont.h>
#include <Qt/qframe.h>
#include <Qt/qgridlayout.h>
#include <Qt/qicon.h>
#include <Qt/qimage.h>
#include <Qt/qmdiarea.h>
#include <Qt/qmdisubwindow.h>
#include <Qt/qmenu.h>
#include <Qt/qmenubar.h>
#include <Qt/qmessagebox.h>
#include <Qt/qlabel.h>
#include <Qt/qlocale.h>
#include <Qt/qlistwidget.h>
#include <Qt/qpainter.h>
#include <Qt/qprogressbar.h>
#include <Qt/qpushbutton.h>
#include <Qt/qslider.h>
#include <Qt/qstandarditemmodel.h>
#include <Qt/qstring.h>
#include <Qt/qstringlist.h>
#include <Qt/qtabwidget.h>
#include <Qt/qtextedit.h>
#include <Qt/qtextstream.h>
#include <Qt/qtimer.h>
#include <Qt/qtranslator.h>
#include <Qt/qtreeview.h>
#include <Qt/qtreewidget.h>
#include <Qt/qurl.h>
#include <Qt/qwidget.h>
#include <Qt/qmainwindow.h>
#include <Qt/qdockwidget.h>
#include <Qt/qinputdialog.h>

#include <QtCore/qtimer.h>
#include <QtCore/qrect.h>
#include <QtCore/qmetatype.h>

#include <QtGui/qbuttongroup.h>
#include <QtGui/qevent.h>
#include <QtGui/qfiledialog.h>
#include <QtGui/QGraphicsItem>
#include <QtGui/QGraphicsScene>
#include <QtGui/QGraphicsView>
#include <QtGui/qmatrix.h>
#include <QtGui/qpainter.h>
#include <QtGui/qpixmap.h>
#include <QtGui/qstatusbar.h>
#include <QtGui/qtooltip.h>

#include <QtOpenGL/qgl.h>

#include <qwt-qt4/qwt_legend.h>
#include <qwt-qt4/qwt_legend_item.h>
#include <qwt-qt4/qwt_plot.h>
#include <qwt-qt4/qwt_plot_canvas.h>
#include <qwt-qt4/qwt_plot_curve.h>
#include <qwt-qt4/qwt_symbol.h>
#include <qwt-qt4/qwt_wheel.h>

#if defined __SUNPRO_CC
#pragma enable_warn
#elif defined _MSC_VER
#pragma warning(pop)
#endif

Q_DECLARE_METATYPE(int32_t)

#endif /*MONITOR_QTINCLUDES_H_*/

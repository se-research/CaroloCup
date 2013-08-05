/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "QtIncludes.h"

#include "plugins/objxviewer/OBJXViewerWidget.h"

namespace cockpit {
    namespace plugins {
        namespace objxviewer {

            using namespace std;

            OBJXViewerWidget::OBJXViewerWidget(const PlugIn &plugIn, QWidget *prnt) :
                    QWidget(prnt),
                    m_viewerWidget(NULL),
                    m_objxViewerWidget(NULL) {

                // Set size.
                setMinimumSize(640, 480);

                // Layout manager.
                QGridLayout* mainBox = new QGridLayout(this);

                QMenuBar* menubar = new QMenuBar(this);
                QAction* open = new QAction(tr("op&en .objx.."), this);
                open->setShortcut(tr("Ctrl+O"));
                open->setToolTip("Close the application.");
                connect(open, SIGNAL(triggered()), SLOT(openOBJX()));
                menubar->addAction(open);
                mainBox->addWidget(menubar, 0, 0);

                // OpenGL scene viewer.
                m_objxViewerWidget = new OBJXGLWidget(plugIn, this);
                m_objxViewerWidget->setMinimumWidth(540);
                m_objxViewerWidget->setMinimumHeight(300);

                // Controllable frame.
                m_viewerWidget = new GLControlFrame(m_objxViewerWidget);
                mainBox->addWidget(m_viewerWidget, 1, 0);

                // Set layout manager.
                setLayout(mainBox);
            }

            OBJXViewerWidget::~OBJXViewerWidget() {
                OPENDAVINCI_CORE_DELETE_POINTER(m_objxViewerWidget);
                OPENDAVINCI_CORE_DELETE_POINTER(m_viewerWidget);
            }

            void OBJXViewerWidget::openOBJX() {
                QString fileName = QFileDialog::getOpenFileName(this, tr("Open .objx..."), NULL );
                if (!fileName.isEmpty()) {
                    m_objxViewerWidget->setOBJXModel(fileName.toStdString());
                }
            }
        }
    }
} // cockpit::plugins::objxviewer

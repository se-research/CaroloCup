/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "QtIncludes.h"

#include "plugins/scnxviewer/SCNXViewerWidget.h"

namespace cockpit {
    namespace plugins {
        namespace scnxviewer {

            using namespace std;

            SCNXViewerWidget::SCNXViewerWidget(const PlugIn &plugIn, QWidget *prnt) :
                    QWidget(prnt),
                    m_viewerWidget(NULL),
                    m_scnxViewerWidget(NULL) {

                // Set size.
                setMinimumSize(640, 480);

                // Layout manager.
                QGridLayout* mainBox = new QGridLayout(this);

                QMenuBar* menubar = new QMenuBar(this);
                QAction* open = new QAction(tr("op&en .scnx.."), this);
                open->setShortcut(tr("Ctrl+O"));
                open->setToolTip("Close the application.");
                connect(open, SIGNAL(triggered()), SLOT(openSCNX()));
                menubar->addAction(open);
                mainBox->addWidget(menubar, 0, 0);

                // OpenGL scene viewer.
                m_scnxViewerWidget = new SCNXGLWidget(plugIn, this);
                m_scnxViewerWidget->setMinimumWidth(540);
                m_scnxViewerWidget->setMinimumHeight(300);

                // Controllable frame.
                m_viewerWidget = new GLControlFrame(m_scnxViewerWidget);
                mainBox->addWidget(m_viewerWidget, 1, 0);

                // Set layout manager.
                setLayout(mainBox);
            }

            SCNXViewerWidget::~SCNXViewerWidget() {
                OPENDAVINCI_CORE_DELETE_POINTER(m_scnxViewerWidget);
                OPENDAVINCI_CORE_DELETE_POINTER(m_viewerWidget);
            }

            void SCNXViewerWidget::openSCNX() {
                QString fileName = QFileDialog::getOpenFileName(this, tr("Open .scnx..."), NULL );
                if (!fileName.isEmpty()) {
                    m_scnxViewerWidget->setSCNXModel(fileName.toStdString());
                }
            }

        }
    }
} // cockpit::plugins::scnxviewer

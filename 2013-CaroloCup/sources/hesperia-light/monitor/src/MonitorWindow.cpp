/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "MonitorWindow.h"

#include <iostream>
#include <fstream>
#include <cstdlib>

#include "QtIncludes.h"

#include "core/SharedPointer.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"

#include "MdiPlugIn.h"

#include "plugins/objxviewer/OBJXViewerPlugIn.h"
#include "plugins/scnxviewer/SCNXViewerPlugIn.h"
#include "plugins/sharedimageviewer/SharedImageViewerPlugIn.h"

namespace monitor {

    using namespace std;
    using namespace core::base;
    using namespace hesperia::base;

    MonitorWindow::MonitorWindow(const KeyValueConfiguration &kvc,
                                 DataStoreManager &dsm) :
            m_kvc(kvc),
            m_dataStoreManager(dsm),
            m_multiplexer(),
            m_plugInProvider(plugins::PlugInProvider::getInstance(kvc, dsm, this)),
            m_listOfPlugIns(),
            m_monitorArea(),
            m_fileMenu(),
            m_windowMenu(),
            m_availablePlugInsList() {

    	m_multiplexer = new FIFOMultiplexer(dsm);
        constructLayout();

    }

    MonitorWindow::~MonitorWindow() {
        m_multiplexer->stop();
    }

    void
    MonitorWindow::constructLayout() {
        // Setup an MDI application.
        m_monitorArea = new QMdiArea(this);

        // Construct menu.
        m_fileMenu = menuBar()->addMenu(tr("&File"));
        m_fileMenu->addSeparator();
        QAction *closeAction = new QAction(tr("&Close"), this);
        closeAction->setShortcut(tr("Ctrl+Q"));
        closeAction->setToolTip("Close the application.");
        connect(closeAction, SIGNAL(triggered()), this, SLOT(close()));
        m_fileMenu->addAction(closeAction);

        m_windowMenu = menuBar()->addMenu(tr("&Window"));
        QAction* cascadeSWAct = new QAction(tr("C&ascade"), this);
        cascadeSWAct->setShortcut(tr("Ctrl+F9"));
        cascadeSWAct->setToolTip(tr("Cascade all open monitors."));
        connect(cascadeSWAct, SIGNAL(triggered()), m_monitorArea, SLOT(cascadeSubWindows()));
        m_windowMenu->addAction(cascadeSWAct);

        QAction* tileSWAct = new QAction(tr("&Tile"), this);
        tileSWAct->setShortcut(tr("Ctrl+F10"));
        tileSWAct->setToolTip(tr("Tile all open monitors."));
        connect(tileSWAct, SIGNAL(triggered()), m_monitorArea, SLOT(tileSubWindows()));

        m_windowMenu->addAction(tileSWAct);
        m_windowMenu->addSeparator();
        QAction* maxSWAction = new QAction(tr("Ma&ximize"), this);
        maxSWAction->setShortcut(tr("Ctrl+F11"));
        maxSWAction->setToolTip(tr("Maximize current monitor."));
        connect(maxSWAction, SIGNAL(triggered()), SLOT(maximizeActiveSubWindow()));

        m_windowMenu->addAction(maxSWAction);
        QAction* minSWAction = new QAction(tr("M&inimize"), this);
        minSWAction->setShortcut(tr("Ctrl+F12"));
        minSWAction->setToolTip(tr("Minimize current monitor."));
        connect(minSWAction, SIGNAL(triggered()), SLOT(minimizeActiveSubWindow()));

        m_windowMenu->addAction(minSWAction);
        QAction* resetSWAction = new QAction(tr("&Reset"), this);
        resetSWAction->setShortcut(tr("Ctrl+Shift+F12"));
        resetSWAction->setToolTip(tr("Reset current monitor."));
        connect(resetSWAction, SIGNAL(triggered()), SLOT(resetActiveSubWindow()));

        m_windowMenu->addAction(resetSWAction);
        QAction* closeSWAction = new QAction(tr("&Close"), this);
        closeSWAction->setShortcut(tr("Ctrl+C"));
        closeSWAction->setToolTip(tr("Close current monitor."));
        connect(closeSWAction, SIGNAL(triggered()), m_monitorArea , SLOT(closeActiveSubWindow()));

        m_windowMenu->addAction(closeSWAction);
        QAction* closeAllSWAction = new QAction(tr("Close &all"), this);
        closeAllSWAction->setShortcut(tr("Ctrl+Shift+C"));
        closeAllSWAction->setToolTip(tr("Close all monitors."));
        connect(closeAllSWAction, SIGNAL(triggered()), m_monitorArea, SLOT(closeAllSubWindows()));
        m_windowMenu->addAction(closeAllSWAction);


        // List all available plugins.
        m_availablePlugInsList = new QListWidget(this);
        m_availablePlugInsList->setMaximumWidth(200);
        m_availablePlugInsList->setMinimumWidth(200);

        QDockWidget *dockWidget = new QDockWidget(tr("Plugins"), this);
        dockWidget->setAllowedAreas(Qt::LeftDockWidgetArea |
                                    Qt::RightDockWidgetArea);

        // Query PlugInProvider for available plugins.
        vector<string> listOfAvailablePlugins = m_plugInProvider.getListOfAvailablePlugIns();
        vector<string>::iterator it = listOfAvailablePlugins.begin();
        while (it != listOfAvailablePlugins.end()) {
            QListWidgetItem *item = new QListWidgetItem(m_availablePlugInsList);
            item->setText((*it).c_str());
            item->setToolTip(m_plugInProvider.getDescriptionForPlugin((*it)).c_str());
            it++;
        }

        connect(m_availablePlugInsList, SIGNAL(itemDoubleClicked(QListWidgetItem*)), SLOT(showPlugIn(QListWidgetItem*)));

        // Choose master plugin.
        dockWidget->setWidget(m_availablePlugInsList);
        addDockWidget(Qt::LeftDockWidgetArea, dockWidget);

        /*
         * Ask for MasterPlugIn and set chosen one as DockWidget
         */
/*
        QStringList items;
        vector<string> listOfMasterPlugIns = m_plugInProvider.getListOfAvailableMasterPlugIns();
        it = listOfMasterPlugIns.begin();
        while (it != listOfMasterPlugIns.end()) {
            items << ((*it).c_str());
            it++;
        }
        bool ok = false;
        QString selectedMasterPlugIn = QInputDialog::getItem(this,tr("Select Master PlugIn"),tr("PlugIn"),items,0,false,&ok);
        if (ok && !selectedMasterPlugIn.isEmpty()) {
          QDockWidget *masterPlugInDock = new QDockWidget(selectedMasterPlugIn,this);
          masterPlugInDock->setAllowedAreas(Qt::BottomDockWidgetArea | Qt::TopDockWidgetArea);
          core::SharedPointer<plugins::MasterPlugIn> masterPlugIn = m_plugInProvider.getMasterPlugIn(selectedMasterPlugIn.toStdString());
          masterPlugInDock->setWidget(masterPlugIn->getQWidget());
          masterPlugInDock->setObjectName(QString(masterPlugIn->getName().c_str()));
          addDockWidget(Qt::BottomDockWidgetArea, masterPlugInDock);
          m_multiplexer = masterPlugIn->getMultiplexer();
        }
*/

        m_multiplexer->start();

        setCentralWidget(m_monitorArea);
    }

    void MonitorWindow::maximizeActiveSubWindow() {
        if (m_monitorArea->activeSubWindow() != 0) {
            m_monitorArea->activeSubWindow()->showMaximized();
        }
    }

    void MonitorWindow::minimizeActiveSubWindow() {
        if (m_monitorArea->activeSubWindow() != 0) {
            m_monitorArea->activeSubWindow()->showMinimized();
        }
    }

    void MonitorWindow::resetActiveSubWindow() {
        if (m_monitorArea->activeSubWindow() != 0) {
            m_monitorArea->activeSubWindow()->showNormal();
        }
    }

    void MonitorWindow::close() {
        if (m_monitorArea != NULL) {
            if (m_monitorArea->subWindowList().size() > 0) {
                m_monitorArea->closeAllSubWindows();
            }
        }
        QWidget::close();
    }

    void MonitorWindow::showPlugIn(QListWidgetItem *item) {
        core::SharedPointer<plugins::PlugIn> plugIn = m_plugInProvider.getPlugIn(item->text().toStdString());

        if (plugIn.isValid()) {
            m_listOfPlugIns.push_back(plugIn);

            // Set container observer.
            plugIn->setContainerObserver(m_multiplexer);

            // Setup plugin.
            plugIn->setupPlugin();

            // Show plugin within an MDI window.
            if (plugIn->getQWidget() != NULL) {
                plugIn->getQWidget()->setMinimumWidth(600);
                plugIn->getQWidget()->setMinimumHeight(400);

                MdiPlugIn* subWindow = new MdiPlugIn(*plugIn, m_monitorArea);
                subWindow->setAttribute(Qt::WA_DeleteOnClose);
                subWindow->setObjectName(plugIn->getName().c_str());
                subWindow->setWindowTitle(plugIn->getName().c_str());
                subWindow->setWidget(plugIn->getQWidget());

                m_monitorArea->addSubWindow(subWindow);

                subWindow->show();
            }
        }
    }
}

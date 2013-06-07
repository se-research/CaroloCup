/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>
#include <string>

#include "core/macros.h"
#include "core/base/Lock.h"
#include "core/data/Container.h"
#include "core/io/URL.h"

#include "hesperia/scenario/SCNXArchiveFactory.h"

#include "plugins/surroundingsviewer/SurroundingsViewerWidget.h"

namespace plugins {
    namespace surroundingsviewer {

        using namespace core::base;
        using namespace core::data;
        using namespace core::io;
        using namespace hesperia::data::scenario;
        using namespace hesperia::scenario;

        SurroundingsViewerWidget::SurroundingsViewerWidget(const PlugIn &plugIn, QWidget *prnt) :
                QWidget(prnt),
                m_viewerControlWidget(NULL),
                m_surroundingsViewerGLWidget(NULL),
                m_surroundingsViewerWidget2D(NULL),
                m_cameraSelector(NULL),
                m_backgroundSelector(NULL),
                m_dataRendererWithCarModel(),
                m_dataRenderer(),
                m_scnxArchive(NULL),
                m_scenario(NULL),
                m_scenarioRenderer(),
                m_scene(NULL),
                m_view(NULL) {

            // Load scenario.
            const URL urlOfSCNXFile(plugIn.getKeyValueConfiguration().getValue<string>("global.scenario"));
            if (urlOfSCNXFile.isValid()) {
                m_scnxArchive = &(SCNXArchiveFactory::getInstance().getSCNXArchive(urlOfSCNXFile));

                if (m_scnxArchive != NULL) {
                    m_scenario = &(m_scnxArchive->getScenario());
                    m_scenarioRenderer.setSCNXArchive(m_scnxArchive);
                }
            }

            // Try loading the car model.
            m_dataRendererWithCarModel.setEgoStateModel(plugIn.getKeyValueConfiguration().getValue<string>("global.car"));

            // Set SCNXArchive data.
            m_dataRendererWithCarModel.setSCNXArchive(m_scnxArchive);
            m_dataRenderer.setSCNXArchive(m_scnxArchive);

            QTabWidget *tabWidget = new QTabWidget(this);

            ////////////////////////////////////////////////////////////////////

            // Setup 2D viewer.
            m_surroundingsViewerWidget2D = new SurroundingsViewerWidget2D(&m_dataRenderer, &m_scenarioRenderer, m_scenario);

            m_scene = new QGraphicsScene();
            m_scene->setSceneRect(-50, -50, 50, 50);
            m_scene->setItemIndexMethod(QGraphicsScene::NoIndex);

            // Add 2D viewer to scene.
            m_scene->addItem(m_surroundingsViewerWidget2D);

            m_view = new QGraphicsView(m_scene);
            m_view->setRenderHint(QPainter::Antialiasing);
            m_view->setCacheMode(QGraphicsView::CacheBackground);
            m_view->setViewportUpdateMode(QGraphicsView::SmartViewportUpdate);
            m_view->setDragMode(QGraphicsView::ScrollHandDrag);
            m_view->resize(100, 100);

            m_view->show();
            tabWidget->addTab(m_view, QString("Bird's eye"));

            ////////////////////////////////////////////////////////////////////

            // Setup 3D viewer.
            QGridLayout *surroundingsGrid = new QGridLayout();
            m_surroundingsViewerGLWidget = new SurroundingsViewerGLWidget(plugIn, &m_dataRendererWithCarModel, &m_scenarioRenderer, m_scenario, NULL);
            m_viewerControlWidget = new GLControlFrame(m_surroundingsViewerGLWidget);

            ////////////////////////////////////////////////////////////////////

            // Background control.
            QLabel *lblBackgroundColor = new QLabel(tr("Background:"));
            m_backgroundSelector = new QComboBox(this);
            m_backgroundSelector->addItem(tr("black"));
            m_backgroundSelector->addItem(tr("white"));

            connect(m_backgroundSelector, SIGNAL(currentIndexChanged(const QString &)), this, SLOT(selectedItemChangedBackground(const QString &)));

            // Camera control.
            QLabel *lblCamera = new QLabel(tr("Camera:"));
            m_cameraSelector = new QComboBox(this);
            m_cameraSelector->addItem(tr("Free Camera"));
            m_cameraSelector->addItem(tr("EgoCar"));

            connect(m_cameraSelector, SIGNAL(currentIndexChanged(const QString &)), this, SLOT(selectedItemChangedCamera(const QString &)));

            ////////////////////////////////////////////////////////////////////

            QTreeWidget *textualSceneGraph = new QTreeWidget();
            textualSceneGraph->setMaximumWidth(160);
            textualSceneGraph->setColumnCount(1);

            QStringList headerLabel;
            headerLabel << tr("Surroundings' Elements");
            textualSceneGraph->setColumnWidth(0, 180);
            textualSceneGraph->setHeaderLabels(headerLabel);

            ////////////////////////////////////////////////////////////////////
            QWidget *sideBar = new QWidget();
            QGridLayout *sideBarLayout = new QGridLayout();
            sideBarLayout->addWidget(lblBackgroundColor, 0, 0);
            sideBarLayout->addWidget(m_backgroundSelector, 1, 0);
            sideBarLayout->addWidget(lblCamera, 2, 0);
            sideBarLayout->addWidget(m_cameraSelector, 3, 0);
            sideBarLayout->addWidget(textualSceneGraph, 4, 0);
            sideBar->setLayout(sideBarLayout);

            // Setup layout.
            surroundingsGrid->addWidget(m_viewerControlWidget, 0, 0);
            surroundingsGrid->addWidget(sideBar, 0, 1);
            surroundingsGrid->setColumnStretch(0, 1);
            surroundingsGrid->setColumnStretch(1, 0);

            QWidget *q = new QWidget();
            q->setLayout(surroundingsGrid);
            tabWidget->addTab(q, QString("Surroundings' View"));

            // Add layout for QTabWidget.
            QGridLayout *tabGrid = new QGridLayout();
            tabGrid->addWidget(tabWidget, 0, 0);
            setLayout(tabGrid);
        }

        SurroundingsViewerWidget::~SurroundingsViewerWidget() {}

        void SurroundingsViewerWidget::selectedItemChangedBackground(const QString &item) {
            if (item.toStdString() == "black") {
                m_surroundingsViewerGLWidget->setBackgroundColor(hesperia::data::environment::Point3(0, 0, 0));
            }
            if (item.toStdString() == "white") {
                m_surroundingsViewerGLWidget->setBackgroundColor(hesperia::data::environment::Point3(1, 1, 1));
            }
        }

        void SurroundingsViewerWidget::selectedItemChangedCamera(const QString &item) {
            m_surroundingsViewerGLWidget->assignToEgoState(item.toStdString() == "EgoCar");
        }

        void SurroundingsViewerWidget::nextContainer(Container &c) {
            m_surroundingsViewerWidget2D->nextContainer(c);
            m_surroundingsViewerGLWidget->nextContainer(c);
        }
    }

} // plugins::surroundingsviewer

/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "QtIncludes.h"

#include "core/data/Constants.h"

#include "plugins/GLControlFrame.h"
#include "plugins/AbstractGLWidget.h"

namespace cockpit {
    namespace plugins {

        GLControlFrame::GLControlFrame(AbstractGLWidget* controllableGLWidget) :
                m_hWheel(NULL),
                m_vWheel(NULL) {

            QGridLayout* grid = new QGridLayout(this);
            m_vWheel = new QwtWheel(this);
            m_vWheel->setOrientation(Qt::Vertical);
            m_vWheel->setRange(0, -core::data::Constants::PI, 0.01);
            m_vWheel->setMinimumHeight(300);
            m_vWheel->setMaximumWidth(20);
            connect(m_vWheel, SIGNAL(valueChanged(double)), controllableGLWidget, SLOT(setTheta(double)));
            grid->addWidget(m_vWheel, 1, 0);

            m_hWheel = new QwtWheel(this);
            m_hWheel->setRange(-1, 1, 0.01);
            m_hWheel->setMinimumWidth(300);
            m_hWheel->setMaximumHeight(20);
            m_hWheel->setOrientation(Qt::Horizontal);
            connect(m_hWheel, SIGNAL(valueChanged(double)),  SLOT(newHValue(double)));
            connect(this , SIGNAL(horizontalValueChanged(double)), controllableGLWidget, SLOT(setPhi(double)));
            grid->addWidget(m_hWheel, 3, 2);

            QPushButton* rstVBtn = new QPushButton("", this);
            rstVBtn->setMaximumSize(20, 20);
            connect(rstVBtn, SIGNAL(clicked()), SLOT(resetVValue()));
            grid->addWidget(rstVBtn, 0, 0);

            QPushButton* rstHBtn = new QPushButton("", this);
            rstHBtn->setMaximumSize(20, 20);
            connect(rstHBtn, SIGNAL(clicked()), SLOT(resetHValue()));
            grid->addWidget(rstHBtn, 3, 3);

            grid->addWidget(controllableGLWidget, 0, 1, 3, 3);
            grid->setColumnStretch(1, 1);
            grid->setColumnStretch(3, 1);
            grid->setRowStretch(0, 1);
            grid->setRowStretch(2, 1);
            setLayout(grid);

            connect(controllableGLWidget, SIGNAL(thetaChanged(double)), m_vWheel, SLOT(setValue(double)));
            connect(controllableGLWidget, SIGNAL(phiChanged(double)), m_hWheel, SLOT(setValue(double)));
        }

        GLControlFrame::~GLControlFrame() {}

        void GLControlFrame::newVValue(double newValue) {
            m_vWheel->setRange(newValue - 10, newValue + 10, 1);
            emit verticalValueChanged(newValue);
        }

        void GLControlFrame::newHValue(double newValue) {
            m_hWheel->setRange(newValue - 1, newValue + 1, 0.01);
            emit horizontalValueChanged(newValue);
        }

        void GLControlFrame::resetHValue() {
            m_hWheel->setValue(0);
        }

        void GLControlFrame::resetVValue() {
            m_vWheel->setValue(0);
        }
    }
} // cockpit::plugins


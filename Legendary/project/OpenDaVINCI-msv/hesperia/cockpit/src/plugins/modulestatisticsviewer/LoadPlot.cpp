/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/SharedPointer.h"

#include "QtIncludes.h"
#include "plugins/modulestatisticsviewer/LoadPlot.h"
#include "plugins/modulestatisticsviewer/LoadPerModule.h"

namespace cockpit {
    namespace plugins {
        namespace modulestatisticsviewer {

            LoadPlot::LoadPlot(QWidget* prnt) :
	            QwtPlot(prnt),
	            m_legend(),
	            m_toAdd(),
	            m_toAttachVisitor(){
	            //set parameter
                setCanvasBackground(Qt::white);
                setTitle(QString("ModuleStatistics"));
                setFrameStyle(QFrame::NoFrame);
                setLineWidth(0);
                setAxisTitle(QwtPlot::xBottom, "count");
                setAxisTitle(QwtPlot::yLeft, "load");

                //add legend
                m_legend = new QwtLegend(this);
                m_legend->setFrameStyle(QFrame::Box | QFrame::Sunken);
                m_legend->setItemMode(QwtLegend::ReadOnlyItem);
                insertLegend(m_legend,QwtPlot::BottomLegend);

                m_toAttachVisitor = new QTimer(this);
                connect(m_toAttachVisitor, SIGNAL(timeout()), SLOT(attachQueuedLPM()));
            }

            LoadPlot::~LoadPlot(){}

            void
            LoadPlot::addLoadPerModule(core::SharedPointer<LoadPerModule> lpm) {
	            m_toAdd.push_back(lpm);
	            m_toAttachVisitor->start(100);
            }

            void LoadPlot::attachQueuedLPM(){
	            for (uint32_t i = 0; i < m_toAdd.size(); i++) {
		            m_toAdd[i]->getCurve()->attach(this);
	            }
	            m_toAttachVisitor->stop();
            }
        }
    }
}


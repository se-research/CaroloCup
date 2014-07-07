/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <cmath>
#include <sstream>

#include "core/data/Constants.h"
#include "core/data/Container.h"
#include "core/data/control/ForceControl.h"

#include "QtIncludes.h"

#include "plugins/forcecontrolviewer/ForceControlViewerWidget.h"

namespace cockpit {

    namespace plugins {

        namespace forcecontrolviewer {

            using namespace std;
            using namespace core::base;
            using namespace core::data;
            using namespace core::data::control;

            ForceControlViewerWidget::ForceControlViewerWidget(const PlugIn &/*plugIn*/, const core::base::KeyValueConfiguration &/*kvc*/, QWidget *prnt) :
                QWidget(prnt),
#ifndef PANDABOARD
                m_plotAcceleration(NULL),
                m_curveAcceleration(NULL),
                m_plotBrake(NULL),
                m_curveBrake(NULL),
                m_plotSteering(NULL),
                m_curveSteering(NULL),
#endif
                m_data()
#ifndef PANDABOARD
                , m_accelerationData(m_data, ForceControlData::ACCELERATION_FORCE),
                m_brakeData(m_data, ForceControlData::BRAKE_FORCE),
                m_steeringData(m_data, ForceControlData::STEERING_FORCE)
#endif
             {

                // Set size.
                setMinimumSize(640, 480);

#ifndef PANDABOARD
                // Acceleration plot.
                m_plotAcceleration = new QwtPlot(this);
                m_plotAcceleration->setCanvasBackground(Qt::white);
                m_plotAcceleration->setFrameStyle(QFrame::NoFrame);
                m_plotAcceleration->setLineWidth(0);
                m_plotAcceleration->setAxisTitle(QwtPlot::xBottom, "t");
                m_plotAcceleration->setAxisTitle(QwtPlot::yLeft, "acceleration force");

                m_curveAcceleration = new QwtPlotCurve();
                m_curveAcceleration->setRenderHint(QwtPlotItem::RenderAntialiased);
                m_curveAcceleration->setData(m_accelerationData);
                m_curveAcceleration->attach(m_plotAcceleration);

                // Brake plot.
                m_plotBrake = new QwtPlot(this);
                m_plotBrake->setCanvasBackground(Qt::white);
                m_plotBrake->setFrameStyle(QFrame::NoFrame);
                m_plotBrake->setLineWidth(0);
                m_plotBrake->setAxisTitle(QwtPlot::xBottom, "t");
                m_plotBrake->setAxisTitle(QwtPlot::yLeft, "brake force");

                m_curveBrake = new QwtPlotCurve();
                m_curveBrake->setRenderHint(QwtPlotItem::RenderAntialiased);
                m_curveBrake->setData(m_brakeData);
                m_curveBrake->attach(m_plotBrake);

                // Steering plot.
                m_plotSteering = new QwtPlot(this);
                m_plotSteering->setCanvasBackground(Qt::white);
                m_plotSteering->setFrameStyle(QFrame::NoFrame);
                m_plotSteering->setLineWidth(0);
                m_plotSteering->setAxisTitle(QwtPlot::xBottom, "t");
                m_plotSteering->setAxisTitle(QwtPlot::yLeft, "steering force");

                m_curveSteering = new QwtPlotCurve();
                m_curveSteering->setRenderHint(QwtPlotItem::RenderAntialiased);
                m_curveSteering->setData(m_steeringData);
                m_curveSteering->attach(m_plotSteering);

                // Combine all plots.
                QHBoxLayout *plotsLayout = new QHBoxLayout();
                plotsLayout->addWidget(m_plotAcceleration);
                plotsLayout->addWidget(m_plotBrake);
                plotsLayout->addWidget(m_plotSteering);

                setLayout(plotsLayout);
#endif
                // Timer for sending data regularly.
                QTimer* timer = new QTimer(this);
                connect(timer, SIGNAL(timeout()), this, SLOT(TimerEvent()));
                timer->start(50);
            }

            ForceControlViewerWidget::~ForceControlViewerWidget() {}

            void ForceControlViewerWidget::TimerEvent() {
#ifndef PANDABOARD
                m_plotAcceleration->replot();
                m_plotBrake->replot();
                m_plotSteering->replot();
#endif
            }

            void ForceControlViewerWidget::nextContainer(Container &container) {
                if (container.getDataType() == Container::FORCECONTROL) {
                    ForceControl fc = container.getData<ForceControl>();

                    m_data.push_back(fc);

                    if (m_data.size() > 10*60) {
                        m_data.pop_front();
                    }
                }
            }

        }
    }
}


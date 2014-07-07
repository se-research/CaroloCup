/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_FORCECONTROLVIEWERWIDGET_H_
#define COCKPIT_PLUGINS_FORCECONTROLVIEWERWIDGET_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <deque>

#include "core/base/KeyValueConfiguration.h"
#include "core/data/Container.h"
#include "core/data/control/ForceControl.h"
#include "core/io/ContainerListener.h"

#include "QtIncludes.h"

#include "plugins/PlugIn.h"
#include "plugins/forcecontrolviewer/ForceControlData.h"

namespace cockpit {

    namespace plugins {

      namespace forcecontrolviewer {

          using namespace std;

            /**
             * This class is the container for the controller widget.
             */
            class ForceControlViewerWidget : public QWidget, public core::io::ContainerListener {

                Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ForceControlViewerWidget(const ForceControlViewerWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ForceControlViewerWidget& operator=(const ForceControlViewerWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param kvc KeyValueConfiguration for this based widget.
                     * @param prnt Pointer to the parental widget.
                     */
                    ForceControlViewerWidget(const PlugIn &plugIn, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                    virtual ~ForceControlViewerWidget();

                    virtual void nextContainer(core::data::Container &c);

                public slots:
                    void TimerEvent();

                private:
#ifndef PANDABOARD
                    QwtPlot *m_plotAcceleration;
                    QwtPlotCurve *m_curveAcceleration;

                    QwtPlot *m_plotBrake;
                    QwtPlotCurve *m_curveBrake;

                    QwtPlot *m_plotSteering;
                    QwtPlotCurve *m_curveSteering;
#endif
                    deque<core::data::control::ForceControl> m_data;

#ifndef PANDABOARD
                    ForceControlData m_accelerationData;
                    ForceControlData m_brakeData;
                    ForceControlData m_steeringData;
#endif
            };
        }
    }
}

#endif /*COCKPIT_PLUGINS_FORCECONTROLVIEWERWIDGET_H_*/


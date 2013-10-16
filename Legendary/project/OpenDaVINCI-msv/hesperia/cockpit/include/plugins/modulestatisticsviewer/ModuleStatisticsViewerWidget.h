/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef COCKPIT_PLUGINS_MODULESTATISTICSVIEWER_MODULESTATISTICSVIEWERWIDGET_H_
#define COCKPIT_PLUGINS_MODULESTATISTICSVIEWER_MODULESTATISTICSVIEWERWIDGET_H_

#include <deque>
#include <map>

#include "QtIncludes.h"

#include "core/SharedPointer.h"
#include "core/io/ContainerListener.h"
#include "core/data/dmcp/ModuleDescriptor.h"
#include "core/data/dmcp/ModuleDescriptorComparator.h"
#include "core/data/dmcp/ModuleStatistics.h"

#include "plugins/PlugIn.h"

#include "plugins/modulestatisticsviewer/LoadPerModule.h"
#include "plugins/modulestatisticsviewer/LoadPlot.h"

namespace cockpit {
    namespace plugins {
      namespace modulestatisticsviewer {

        using namespace std;

        /**
         * This class is the container for the shared image viewer widget.
         */
        class ModuleStatisticsViewerWidget : public QWidget,
            public core::io::ContainerListener
        {

        Q_OBJECT

        private:
          /**
           * "Forbidden" copy constructor. Goal: The compiler should warn
           * already at compile time for unwanted bugs caused by any misuse
           * of the copy constructor.
           */
          ModuleStatisticsViewerWidget(const ModuleStatisticsViewerWidget &/*obj*/);

          /**
           * "Forbidden" assignment operator. Goal: The compiler should warn
           * already at compile time for unwanted bugs caused by any misuse
           * of the assignment operator.
           */
          ModuleStatisticsViewerWidget&
          operator=(const ModuleStatisticsViewerWidget &/*obj*/);

        public:
          /**
           * Constructor.
           *
           * @param plugIn Reference to the plugin to which this widget belongs.
           * @param prnt Pointer to the parental widget.
           */
          ModuleStatisticsViewerWidget(const PlugIn &plugIn, QWidget *prnt);

          virtual
          ~ModuleStatisticsViewerWidget();

          virtual void
          nextContainer(core::data::Container &c);

        private:
          LoadPlot *m_plot;
          deque<core::data::dmcp::ModuleStatistics> m_moduleStatistics;
          map<core::data::dmcp::ModuleDescriptor, core::SharedPointer<
              LoadPerModule>, core::data::dmcp::ModuleDescriptorComparator>
              m_loadPerModule;
          uint32_t m_color;
        };
    }
  }
} // cockpit::plugins::modulestatisticsviewer

#endif /*COCKPIT_PLUGINS_MODULESTATISTICSVIEWER_MODULESTATISTICSVIEWERWIDGET_H_*/

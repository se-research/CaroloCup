/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MONITOR_PLUGINS_MODULESTATISTICSVIEWER_MODULESTATISTICSVIEWERWIDGET_H_
#define MONITOR_PLUGINS_MODULESTATISTICSVIEWER_MODULESTATISTICSVIEWERWIDGET_H_

#include <deque>
#include <map>

#include "QtIncludes.h"

#include "core/SharedPointer.h"
#include "core/io/ContainerListener.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"
#include "hesperia/data/dmcp/ModuleDescriptorComparator.h"
#include "hesperia/data/dmcp/ModuleStatistics.h"

#include "plugins/PlugIn.h"

#include "plugins/modulestatisticsviewer/LoadPerModule.h"
#include "plugins/modulestatisticsviewer/LoadPlot.h"

namespace plugins
{
  namespace modulestatisticsviewer
  {

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
      deque<hesperia::data::dmcp::ModuleStatistics> m_moduleStatistics;
      map<hesperia::data::dmcp::ModuleDescriptor, core::SharedPointer<
          LoadPerModule>, hesperia::data::dmcp::ModuleDescriptorComparator>
          m_loadPerModule;
      uint32_t m_color;
    };

  }
} // plugins::modulestatisticsviewer

#endif /*MONITOR_PLUGINS_MODULESTATISTICSVIEWER_MODULESTATISTICSVIEWERWIDGET_H_*/

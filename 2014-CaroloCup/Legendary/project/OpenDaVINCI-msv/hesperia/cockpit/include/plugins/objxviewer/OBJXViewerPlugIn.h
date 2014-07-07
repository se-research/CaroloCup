/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef COCKPIT_PLUGINS_OBJXVIEWER_OBJXVIEWERPLUGIN_H_
#define COCKPIT_PLUGINS_OBJXVIEWER_OBJXVIEWERPLUGIN_H_

#include "QtIncludes.h"

#include "core/base/KeyValueConfiguration.h"

#include "plugins/objxviewer/OBJXViewerWidget.h"

namespace cockpit {
    namespace plugins {
      namespace objxviewer {

        using namespace std;

        class OBJXViewerPlugIn : public PlugIn
        {

        private:
          /**
           * "Forbidden" copy constructor. Goal: The compiler should warn
           * already at compile time for unwanted bugs caused by any misuse
           * of the copy constructor.
           */
          OBJXViewerPlugIn(const OBJXViewerPlugIn &/*obj*/);

          /**
           * "Forbidden" assignment operator. Goal: The compiler should warn
           * already at compile time for unwanted bugs caused by any misuse
           * of the assignment operator.
           */
          OBJXViewerPlugIn&
          operator=(const OBJXViewerPlugIn &/*obj*/);

        public:
          /**
           * Constructor.
           *
           * @param name Name of this plugin.
           * @param kvc KeyValueConfiguration for this GL-based widget.
           * @param prnt Pointer to the container super window.
           */
          OBJXViewerPlugIn(const string &name,
              const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

          virtual
          ~OBJXViewerPlugIn();

          virtual QWidget*
          getQWidget() const;

          virtual void
          setupPlugin();

          virtual void
          stopPlugin();

        private:
          OBJXViewerWidget *m_viewerWidget;
        };
    }
  }
} // cockpit::plugins::objxviewer

#endif /*COCKPIT_PLUGINS_OBJXVIEWER_OBJXVIEWERPLUGIN_H_*/

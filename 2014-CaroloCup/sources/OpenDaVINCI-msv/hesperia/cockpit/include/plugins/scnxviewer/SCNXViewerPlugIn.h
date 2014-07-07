/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef COCKPIT_PLUGINS_SCNXVIEWER_SCNXVIEWERPLUGIN_H_
#define COCKPIT_PLUGINS_SCNXVIEWER_SCNXVIEWERPLUGIN_H_

#include "QtIncludes.h"

#include "core/base/KeyValueConfiguration.h"

#include "plugins/scnxviewer/SCNXViewerWidget.h"

namespace cockpit {
    namespace plugins {
      namespace scnxviewer {

        using namespace std;

        class SCNXViewerPlugIn : public PlugIn {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SCNXViewerPlugIn(const SCNXViewerPlugIn &/*scn*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SCNXViewerPlugIn& operator=(const SCNXViewerPlugIn &/*scn*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param name Name of this plugin.
                 * @param kvc KeyValueConfiguration for this GL-based widget.
                 * @param prnt Pointer to the container super window.
                 */
                SCNXViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                virtual ~SCNXViewerPlugIn();

                virtual QWidget* getQWidget() const;

                virtual void setupPlugin();

                virtual void stopPlugin();

            private:
                SCNXViewerWidget *m_viewerWidget;
        };

      }
    }
} // cockpit::plugins::scnxviewer

#endif /*COCKPIT_PLUGINS_SCNXVIEWER_SCNXVIEWERPLUGIN_H_*/

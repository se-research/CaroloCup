/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MONITOR_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERPLUGIN_H_
#define MONITOR_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERPLUGIN_H_

#include "core/base/KeyValueConfiguration.h"

#include "plugins/PlugIn.h"
#include "plugins/sharedimageviewer/SharedImageViewerWidget.h"

namespace plugins {
    namespace sharedimageviewer {

        using namespace std;

        class SharedImageViewerPlugIn : public PlugIn {

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                SharedImageViewerPlugIn(const SharedImageViewerPlugIn &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                SharedImageViewerPlugIn& operator=(const SharedImageViewerPlugIn &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param name Name of this plugin.
                 * @param kvc KeyValueConfiguration for this widget.
                 * @param prnt Pointer to the container super window.
                 */
                SharedImageViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt);

                virtual ~SharedImageViewerPlugIn();

                virtual QWidget* getQWidget() const;

                virtual void setupPlugin();

                virtual void stopPlugin();

            private:
                SharedImageViewerWidget *m_imageViewerWidget;
        };

    }
} // plugins::sharedimageviewer

#endif /*MONITOR_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERPLUGIN_H_*/

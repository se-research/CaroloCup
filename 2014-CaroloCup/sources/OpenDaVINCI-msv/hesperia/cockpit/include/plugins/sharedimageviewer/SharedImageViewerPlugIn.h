/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERPLUGIN_H_
#define COCKPIT_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/KeyValueConfiguration.h"

#include "plugins/PlugIn.h"
#include "plugins/sharedimageviewer/SharedImageViewerWidget.h"

namespace cockpit {

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
    }
}

#endif /*COCKPIT_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERPLUGIN_H_*/


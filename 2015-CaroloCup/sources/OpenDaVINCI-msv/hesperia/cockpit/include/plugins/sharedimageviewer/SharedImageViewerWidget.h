/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERWIDGET_H_
#define COCKPIT_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERWIDGET_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <map>
#include <string>
#include <vector>

#include "QtIncludes.h"

#include "core/SharedPointer.h"
#include "core/base/Mutex.h"
#include "core/io/ContainerListener.h"
#include "core/wrapper/SharedMemory.h"
#include "core/data/image/SharedImage.h"

#include "plugins/PlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace sharedimageviewer {

            using namespace std;

            /**
             * This class is the container for the shared image viewer widget.
             */
            class SharedImageViewerWidget : public QWidget, public core::io::ContainerListener {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    SharedImageViewerWidget(const SharedImageViewerWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    SharedImageViewerWidget& operator=(const SharedImageViewerWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param prnt Pointer to the parental widget.
                     */
                    SharedImageViewerWidget(const PlugIn &plugIn, QWidget *prnt);

                    virtual ~SharedImageViewerWidget();

                    virtual void nextContainer(core::data::Container &c);

                public slots:
				    void selectedSharedImage(QListWidgetItem *item);

                private:
                    mutable core::base::Mutex m_sharedImageMemoryMutex;
                    core::data::image::SharedImage m_sharedImage;
                    core::SharedPointer<core::wrapper::SharedMemory> m_sharedImageMemory;
                    QImage *m_drawableImage;

                    QListWidget *m_list;
                    vector<string> m_listOfAvailableSharedImages;
                    map<string, core::data::image::SharedImage> m_mapOfAvailableSharedImages;

                    virtual void paintEvent(QPaintEvent *evnt);
            };

        }
    }
}

#endif /*COCKPIT_PLUGINS_SHAREDIMAGEVIEWER_SHAREDIMAGEVIEWERWIDGET_H_*/


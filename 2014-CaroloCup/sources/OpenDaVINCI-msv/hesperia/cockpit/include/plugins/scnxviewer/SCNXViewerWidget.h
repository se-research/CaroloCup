/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef COCKPIT_PLUGINS_SCNXVIEWER_SCNXVIEWERWIDGET_H_
#define COCKPIT_PLUGINS_SCNXVIEWER_SCNXVIEWERWIDGET_H_

#include "QtIncludes.h"

#include "plugins/GLControlFrame.h"
#include "plugins/scnxviewer/SCNXGLWidget.h"

namespace cockpit {
    namespace plugins {
        namespace scnxviewer {

            using namespace std;

            /**
             * This class is the container for the SCNX viewer widget.
             */
            class SCNXViewerWidget : public QWidget {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    SCNXViewerWidget(const SCNXViewerWidget &/*scn*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    SCNXViewerWidget& operator=(const SCNXViewerWidget &/*scn*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param prnt Pointer to the parental widget.
                     */
                    SCNXViewerWidget(const PlugIn &plugIn, QWidget *prnt);

                    virtual ~SCNXViewerWidget();

                private slots:
                    /**
                     * This method is called when the user clicks on the button.
                     */
                    void openSCNX();

                private:
                    GLControlFrame *m_viewerWidget;
                    SCNXGLWidget *m_scnxViewerWidget;
            };

        }
    }
} // plugins::scnxviewer

#endif /*COCKPIT_PLUGINS_SCNXVIEWER_SCNXVIEWERWIDGET_H_*/

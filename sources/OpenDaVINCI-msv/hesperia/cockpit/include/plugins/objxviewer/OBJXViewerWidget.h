/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef COCKPIT_PLUGINS_OBJXVIEWER_OBJXVIEWERWIDGET_H_
#define COCKPIT_PLUGINS_OBJXVIEWER_OBJXVIEWERWIDGET_H_

#include "QtIncludes.h"

#include "plugins/GLControlFrame.h"
#include "plugins/objxviewer/OBJXGLWidget.h"

namespace cockpit {
    namespace plugins {
        namespace objxviewer {

            using namespace std;

            /**
             * This class is the container for the OBJX viewer widget.
             */
            class OBJXViewerWidget : public QWidget {

                    Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    OBJXViewerWidget(const OBJXViewerWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    OBJXViewerWidget& operator=(const OBJXViewerWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param prnt Pointer to the parental widget.
                     */
                    OBJXViewerWidget(const PlugIn &plugIn, QWidget *prnt);

                    virtual ~OBJXViewerWidget();

                private slots:
                    /**
                     * This method is called when the user clicks on the button.
                     */
                    void openOBJX();

                private:
                    GLControlFrame *m_viewerWidget;
                    OBJXGLWidget *m_objxViewerWidget;
            };
        }
    }
} // cockpit::plugins::objxviewer

#endif /*COCKPIT_PLUGINS_OBJXVIEWER_OBJXVIEWERWIDGET_H_*/

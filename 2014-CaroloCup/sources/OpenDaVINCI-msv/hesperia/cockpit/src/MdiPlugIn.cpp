/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <iostream>

#include "MdiPlugIn.h"

namespace cockpit {

    using namespace std;

    MdiPlugIn::MdiPlugIn(plugins::PlugIn &plugIn, QWidget *prnt, Qt::WindowFlags flags) :
        QMdiSubWindow(prnt, flags),
        m_plugIn(plugIn) {}

    MdiPlugIn::~MdiPlugIn() {}

    void MdiPlugIn::closeEvent(QCloseEvent *evt) {
        // Stop plugin.
        if ( (evt != NULL) && (evt->type() == QEvent::Close) ) {
            m_plugIn.stopPlugin();
        }

        // Call closeEvent from upper class.
        QMdiSubWindow::closeEvent(evt);
    }

} // cockpit

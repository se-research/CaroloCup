/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>

#include "MdiPlugIn.h"

namespace monitor {

    using namespace std;

    MdiPlugIn::MdiPlugIn(plugins::PlugIn &plugIn, QWidget *prnt, Qt::WindowFlags flags) :
            QMdiSubWindow(prnt, flags),
            m_plugIn(plugIn) {}

    MdiPlugIn::~MdiPlugIn() {}

    void MdiPlugIn::closeEvent(QCloseEvent *evnt) {
        // Stop plugin.
        if ( (evnt != NULL) && (evnt->type() == QEvent::Close) ) {
            m_plugIn.stopPlugin();
        }

        // Call closeEvent from upper class.
        QMdiSubWindow::closeEvent(evnt);
    }

} // monitor

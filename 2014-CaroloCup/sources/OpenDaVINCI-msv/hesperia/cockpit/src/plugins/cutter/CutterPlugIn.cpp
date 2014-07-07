/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/cutter/CutterPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace cutter {

            CutterPlugIn::CutterPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                PlugIn(name, kvc, prnt),
                m_cutterWidget(NULL) {
                setDescription("This plugin displays all distributed containers.");
            }

            CutterPlugIn::~CutterPlugIn() {}

            void CutterPlugIn::setupPlugin() {
                m_cutterWidget = new CutterWidget(*this, getParentQWidget());
            }

            void CutterPlugIn::stopPlugin() {}

            QWidget* CutterPlugIn::getQWidget() const {
                return m_cutterWidget;
            }
        }
    }
}

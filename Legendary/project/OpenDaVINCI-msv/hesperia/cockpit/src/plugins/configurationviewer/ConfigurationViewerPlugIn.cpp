/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/configurationviewer/ConfigurationViewerPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace configurationviewer {

            ConfigurationViewerPlugIn::ConfigurationViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                PlugIn(name, kvc, prnt),
                m_viewerWidget(NULL) {
                setDescription("This plugin displays the current configuration.");
            }

            ConfigurationViewerPlugIn::~ConfigurationViewerPlugIn() {}

            void ConfigurationViewerPlugIn::setupPlugin() {
                m_viewerWidget = new ConfigurationViewerWidget(*this, getKeyValueConfiguration(), getParentQWidget());
            }

            void ConfigurationViewerPlugIn::stopPlugin() {}

            QWidget* ConfigurationViewerPlugIn::getQWidget() const {
                return m_viewerWidget;
            }
        }
    }
}


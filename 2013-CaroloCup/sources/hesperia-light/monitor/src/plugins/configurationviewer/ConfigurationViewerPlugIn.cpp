/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/configurationviewer/ConfigurationViewerPlugIn.h"

namespace plugins {
    namespace configurationviewer {

        ConfigurationViewerPlugIn::ConfigurationViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
            PlugIn(name, kvc, prnt),
            m_viewerWidget(NULL) {
            setDescription("This plugin shows the current configuration.");
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

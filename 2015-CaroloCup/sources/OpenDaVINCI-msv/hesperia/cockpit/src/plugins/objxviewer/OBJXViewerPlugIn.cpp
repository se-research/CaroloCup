/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"

#include "plugins/objxviewer/OBJXViewerPlugIn.h"

namespace cockpit {
    namespace plugins {
        namespace objxviewer {

            OBJXViewerPlugIn::OBJXViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                PlugIn(name, kvc, prnt),
                m_viewerWidget(NULL) {
                setDescription("This plugin shows .objx files.");
            }

            OBJXViewerPlugIn::~OBJXViewerPlugIn() {}

            void OBJXViewerPlugIn::setupPlugin() {
                m_viewerWidget = new OBJXViewerWidget(*this, getParentQWidget());
            }

            void OBJXViewerPlugIn::stopPlugin() {}

            QWidget* OBJXViewerPlugIn::getQWidget() const {
                return m_viewerWidget;
            }

        }
    }
} // cockpit::plugins::objxviewer

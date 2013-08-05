/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"

#include "plugins/scnxviewer/SCNXViewerPlugIn.h"

namespace cockpit {
    namespace plugins {
        namespace scnxviewer {

          SCNXViewerPlugIn::SCNXViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
              PlugIn(name, kvc, prnt),
              m_viewerWidget(NULL) {
              setDescription("This plugin shows SCNX-Archives.");
          }

        SCNXViewerPlugIn::~SCNXViewerPlugIn() {}

        void SCNXViewerPlugIn::setupPlugin() {
            m_viewerWidget = new SCNXViewerWidget(*this, getParentQWidget());
        }

        void SCNXViewerPlugIn::stopPlugin() {}

        QWidget* SCNXViewerPlugIn::getQWidget() const {
            return m_viewerWidget;
        }

      }
    }
} // plugins::scnxviewer

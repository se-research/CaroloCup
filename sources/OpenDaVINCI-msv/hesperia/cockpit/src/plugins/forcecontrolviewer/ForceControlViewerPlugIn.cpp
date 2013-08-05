/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/forcecontrolviewer/ForceControlViewerPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace forcecontrolviewer {

            using namespace core::io;

            ForceControlViewerPlugIn::ForceControlViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                PlugIn(name, kvc, prnt),
                m_fcViewerWidget(NULL) {
                setDescription("This plugin displays the values of ForceControl over time.");
            }

            ForceControlViewerPlugIn::~ForceControlViewerPlugIn() {}

            void ForceControlViewerPlugIn::setupPlugin() {
                m_fcViewerWidget = new ForceControlViewerWidget(*this, getKeyValueConfiguration(), getParentQWidget());

                ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_fcViewerWidget);
                }

            }

            void ForceControlViewerPlugIn::stopPlugin() {
                ContainerObserver *co = getContainerObserver();

                if (co != NULL) {
                    co->removeContainerListener(m_fcViewerWidget);
                }
            }

            QWidget* ForceControlViewerPlugIn::getQWidget() const {
                return m_fcViewerWidget;
            }
        }
    }
}


/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/spy/SpyPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace spy {

            SpyPlugIn::SpyPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                PlugIn(name, kvc, prnt),
                m_viewerWidget(NULL) {
                setDescription("This plugin displays all distributed containers.");
            }

            SpyPlugIn::~SpyPlugIn() {}

            void SpyPlugIn::setupPlugin() {
                m_viewerWidget = new SpyWidget(*this, getParentQWidget());

                ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_viewerWidget);
                }
            }

            void SpyPlugIn::stopPlugin() {
                ContainerObserver *co = getContainerObserver();

                if (co != NULL) {
                    co->removeContainerListener(m_viewerWidget);
                }
            }

            QWidget* SpyPlugIn::getQWidget() const {
                return m_viewerWidget;
            }
        }
    }
}

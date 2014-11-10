/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/ecuviewer/ECUPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace ecuviewer {

            using namespace core::io; 

            ECUPlugIn::ECUPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, ContainerConference &conf, QWidget *prnt) :
                ControlPlugIn(name, kvc, conf, prnt),
                m_viewerWidget(NULL) {
                setDescription("This plugin displays data from the ECU.");
            }

            ECUPlugIn::~ECUPlugIn() {}

            void ECUPlugIn::setupPlugin() {
                m_viewerWidget = new ECUWidget(*this, getConference(), getParentQWidget());

                ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_viewerWidget);
                }
            }

            void ECUPlugIn::stopPlugin() {
                ContainerObserver *co = getContainerObserver();

                if (co != NULL) {
                    co->removeContainerListener(m_viewerWidget);
                }
            }

            QWidget* ECUPlugIn::getQWidget() const {
                return m_viewerWidget;
            }
        }
    }
}

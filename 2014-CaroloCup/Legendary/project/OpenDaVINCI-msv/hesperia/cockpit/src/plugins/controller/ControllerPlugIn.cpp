/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/controller/ControllerPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace controller {

            using namespace core::io;

            ControllerPlugIn::ControllerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, ContainerConference &conf, QWidget *prnt) :
                ControlPlugIn(name, kvc, conf, prnt),
                m_controllerWidget(NULL) {
                setDescription("This plugin allows the control of the vehicle by the arrow keys.");
            }

            ControllerPlugIn::~ControllerPlugIn() {}

            void ControllerPlugIn::setupPlugin() {
                m_controllerWidget = new ControllerWidget(*this, getKeyValueConfiguration(), getConference(), getParentQWidget());

                ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_controllerWidget);
                }

            }

            void ControllerPlugIn::stopPlugin() {
                ContainerObserver *co = getContainerObserver();

                if (co != NULL) {
                    co->removeContainerListener(m_controllerWidget);
                }
            }

            QWidget* ControllerPlugIn::getQWidget() const {
                return m_controllerWidget;
            }
        }
    }
}


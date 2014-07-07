/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/macros.h"

#include "ContainerObserver.h"
#include "plugins/irusmap/IrUsMapPlugIn.h"
#include "plugins/irusmap/IrUsMapWidgetControl.h"

namespace cockpit {

    namespace plugins {

        namespace irusmap {

            IrUsMapPlugIn::IrUsMapPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                    PlugIn(name, kvc, prnt),
                    m_kvc(kvc),
                    m_irusmapWidgetControl(NULL) {
                setDescription("This plugin displays the current irus readings.");
            }

            IrUsMapPlugIn::~IrUsMapPlugIn() {
                // The widget m_irusmapWidget will be destroyed by Qt.
            }

            void IrUsMapPlugIn::setupPlugin() {
                m_irusmapWidgetControl = new IrUsMapWidgetControl(*this, m_kvc, getParentQWidget());

                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_irusmapWidgetControl);
                }
            }

            void IrUsMapPlugIn::stopPlugin() {
                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->removeContainerListener(m_irusmapWidgetControl);
                }
            }

            QWidget* IrUsMapPlugIn::getQWidget() const {
                return m_irusmapWidgetControl;
            }

        }
    }
}


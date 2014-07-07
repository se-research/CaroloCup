/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/iruscharts/IrUsChartsPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace iruscharts {

            using namespace core::io;

            IrUsChartsPlugIn::IrUsChartsPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                PlugIn(name, kvc, prnt),
                m_irusChartsWidget(NULL) {
                setDescription("This plugin displays the values of SensorBoardData over time.");
            }

            IrUsChartsPlugIn::~IrUsChartsPlugIn() {}

            void IrUsChartsPlugIn::setupPlugin() {
                m_irusChartsWidget = new IrUsChartsWidget(*this, getKeyValueConfiguration(), getParentQWidget());

                ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_irusChartsWidget);
                }

            }

            void IrUsChartsPlugIn::stopPlugin() {
                ContainerObserver *co = getContainerObserver();

                if (co != NULL) {
                    co->removeContainerListener(m_irusChartsWidget);
                }
            }

            QWidget* IrUsChartsPlugIn::getQWidget() const {
                return m_irusChartsWidget;
            }
        }
    }
}


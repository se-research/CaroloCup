/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"

#include "ContainerObserver.h"
#include "plugins/modulestatisticsviewer/ModuleStatisticsViewerPlugIn.h"
#include "plugins/modulestatisticsviewer/ModuleStatisticsViewerWidget.h"

namespace cockpit {
    namespace plugins {
        namespace modulestatisticsviewer {

            ModuleStatisticsViewerPlugIn::ModuleStatisticsViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                    PlugIn(name, kvc, prnt),
                    m_modulestatisticsViewerWidget(NULL) {
                setDescription("This plugin shows module statistics.");
            }

            ModuleStatisticsViewerPlugIn::~ModuleStatisticsViewerPlugIn() {
                // The widget m_modulestatisticsViewerWidget will be destroyed by Qt.
            }

            void ModuleStatisticsViewerPlugIn::setupPlugin() {
                m_modulestatisticsViewerWidget = new ModuleStatisticsViewerWidget(*this, getParentQWidget());

                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_modulestatisticsViewerWidget);
                }
            }

            void ModuleStatisticsViewerPlugIn::stopPlugin() {
                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->removeContainerListener(m_modulestatisticsViewerWidget);
                }
            }

            QWidget* ModuleStatisticsViewerPlugIn::getQWidget() const {
                return m_modulestatisticsViewerWidget;
            }
        }
    }
} // cockpit::plugins::modulestatistics

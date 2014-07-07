/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"

#include "QtIncludes.h"
#include "ContainerObserver.h"
#include "plugins/environmentviewer/EnvironmentViewerPlugIn.h"

namespace cockpit {
    namespace plugins {
        namespace environmentviewer {

            using namespace core::base;

            EnvironmentViewerPlugIn::EnvironmentViewerPlugIn(const string &name, const KeyValueConfiguration &kvc, QWidget* prnt) :
                    PlugIn(name, kvc, prnt),
                    m_widget(NULL) {
                setDescription("This plugin shows the entire environment in 3D.");
            }

            EnvironmentViewerPlugIn::~EnvironmentViewerPlugIn() {
            }

            void EnvironmentViewerPlugIn::setupPlugin() {
                m_widget = new EnvironmentViewerWidget(*this, getParentQWidget());

                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_widget);
                }
            }

            void EnvironmentViewerPlugIn::stopPlugin() {
                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->removeContainerListener(m_widget);
                }
            }

            QWidget* EnvironmentViewerPlugIn::getQWidget() const {
                return m_widget;
            }
        }
    }
} // plugins::scnxviewer


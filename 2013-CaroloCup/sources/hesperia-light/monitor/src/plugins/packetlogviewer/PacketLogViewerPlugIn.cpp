/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/packetlogviewer/PacketLogViewerPlugIn.h"

namespace plugins {
    namespace packetlogviewer {

        PacketLogViewerPlugIn::PacketLogViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
            PlugIn(name, kvc, prnt),
            m_viewerWidget(NULL) {
            setDescription("This plugin shows all distributed containers.");
        }

        PacketLogViewerPlugIn::~PacketLogViewerPlugIn() {}

        void PacketLogViewerPlugIn::setupPlugin() {
            m_viewerWidget = new PacketLogViewerWidget(*this, getParentQWidget());

            monitor::ContainerObserver *co = getContainerObserver();
            if (co != NULL) {
                co->addContainerListener(m_viewerWidget);
            }
        }

        void PacketLogViewerPlugIn::stopPlugin() {
            monitor::ContainerObserver *co = getContainerObserver();

            if (co != NULL) {
                co->removeContainerListener(m_viewerWidget);
            }
        }

        QWidget* PacketLogViewerPlugIn::getQWidget() const {
            return m_viewerWidget;
        }
    }
}

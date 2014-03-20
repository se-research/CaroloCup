/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"

#include "ContainerObserver.h"
#include "plugins/sharedimageviewer/SharedImageViewerPlugIn.h"
#include "plugins/sharedimageviewer/SharedImageViewerWidget.h"

namespace plugins {
    namespace sharedimageviewer {

        SharedImageViewerPlugIn::SharedImageViewerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                PlugIn(name, kvc, prnt),
                m_imageViewerWidget(NULL) {
            setDescription("This plugin shows shared images.");
        }

        SharedImageViewerPlugIn::~SharedImageViewerPlugIn() {
            // The widget m_imageViewerWidget will be destroyed by Qt.
        }

        void SharedImageViewerPlugIn::setupPlugin() {
            m_imageViewerWidget = new SharedImageViewerWidget(*this, getParentQWidget());

            monitor::ContainerObserver *co = getContainerObserver();
            if (co != NULL) {
                co->addContainerListener(m_imageViewerWidget);
            }
        }

        void SharedImageViewerPlugIn::stopPlugin() {
            monitor::ContainerObserver *co = getContainerObserver();
            if (co != NULL) {
                co->removeContainerListener(m_imageViewerWidget);
            }
        }

        QWidget* SharedImageViewerPlugIn::getQWidget() const {
            return m_imageViewerWidget;
        }

    }
} // plugins::sharedimageviewer

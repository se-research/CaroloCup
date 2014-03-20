/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/macros.h"

#include "QtIncludes.h"
#include "ContainerObserver.h"
#include "plugins/surroundingsviewer/SurroundingsViewerPlugIn.h"

namespace plugins {
    namespace surroundingsviewer {

        using namespace core::base;

        SurroundingsViewerPlugIn::SurroundingsViewerPlugIn(const string &name, const KeyValueConfiguration &kvc, QWidget* prnt) :
                PlugIn(name, kvc, prnt),
                m_widget(NULL) {
            setDescription("This plugin shows the entire surroundings in 3D.");
        }

        SurroundingsViewerPlugIn::~SurroundingsViewerPlugIn() {
        }

        void SurroundingsViewerPlugIn::setupPlugin() {
            m_widget = new SurroundingsViewerWidget(*this, getParentQWidget());

            monitor::ContainerObserver *co = getContainerObserver();
            if (co != NULL) {
                co->addContainerListener(m_widget);
            }
        }

        void SurroundingsViewerPlugIn::stopPlugin() {
            monitor::ContainerObserver *co = getContainerObserver();
            if (co != NULL) {
                co->removeContainerListener(m_widget);
            }
        }

        QWidget* SurroundingsViewerPlugIn::getQWidget() const {
            return m_widget;
        }

    }
} // plugins::scnxviewer


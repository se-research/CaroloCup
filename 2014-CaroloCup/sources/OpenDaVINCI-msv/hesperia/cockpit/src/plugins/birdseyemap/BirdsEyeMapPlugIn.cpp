/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/macros.h"

#include "QtIncludes.h"
#include "ContainerObserver.h"
#include "plugins/birdseyemap/BirdsEyeMapPlugIn.h"

namespace cockpit {
    namespace plugins {
        namespace birdseyemap {

            using namespace core::base;

            BirdsEyeMapPlugIn::BirdsEyeMapPlugIn(const string &name, const KeyValueConfiguration &kvc, QWidget* prnt) :
                    PlugIn(name, kvc, prnt),
                    m_widget(NULL) {
                setDescription("This plugin shows the entire environment in 2D.");
            }

            BirdsEyeMapPlugIn::~BirdsEyeMapPlugIn() {
            }

            void BirdsEyeMapPlugIn::setupPlugin() {
                m_widget = new BirdsEyeMapWidget(*this, getParentQWidget());

                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->addContainerListener(m_widget);
                }
            }

            void BirdsEyeMapPlugIn::stopPlugin() {
                cockpit::ContainerObserver *co = getContainerObserver();
                if (co != NULL) {
                    co->removeContainerListener(m_widget);
                }
            }

            QWidget* BirdsEyeMapPlugIn::getQWidget() const {
                return m_widget;
            }
        }
    }
} // plugins::birdseyemap


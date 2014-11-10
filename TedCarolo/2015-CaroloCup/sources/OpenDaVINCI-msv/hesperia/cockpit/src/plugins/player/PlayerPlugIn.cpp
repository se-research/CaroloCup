/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/player/PlayerPlugIn.h"

namespace cockpit {

    namespace plugins {

        namespace player {

            PlayerPlugIn::PlayerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, core::io::ContainerConference &conf, QWidget *prnt) :
                ControlPlugIn(name, kvc, conf, prnt),
                m_kvc(kvc),
                m_playerWidget(NULL) {
                setDescription("This plugin replays previously recorded files.");
            }

            PlayerPlugIn::~PlayerPlugIn() {}

            void PlayerPlugIn::setupPlugin() {
                m_playerWidget = new PlayerWidget(*this, m_kvc, getConference(), getParentQWidget());
            }

            void PlayerPlugIn::stopPlugin() {}

            QWidget* PlayerPlugIn::getQWidget() const {
                return m_playerWidget;
            }
        }
    }
}

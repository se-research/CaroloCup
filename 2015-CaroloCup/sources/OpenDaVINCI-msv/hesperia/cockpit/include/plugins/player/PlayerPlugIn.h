/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPIT_PLUGINS_PLAYERPLUGIN_H_
#define COCKPIT_PLUGINS_PLAYERPLUGIN_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/ControlPlugIn.h"
#include "plugins/player/PlayerWidget.h"

namespace cockpit {

    namespace plugins {

        namespace player {

            class PlayerPlugIn : public ControlPlugIn {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    PlayerPlugIn(const PlayerPlugIn &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    PlayerPlugIn& operator=(const PlayerPlugIn &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param name Name of this plugin.
                     * @param kvc KeyValueConfiguration for this GL-based widget.
                     * @param conf ContainerConference.
                     * @param prnt Pointer to the container super window.
                     */
                    PlayerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, core::io::ContainerConference &conf, QWidget *prnt);

                    virtual ~PlayerPlugIn();

                    virtual QWidget* getQWidget() const;

                    virtual void setupPlugin();

                    virtual void stopPlugin();

                private:
                    const core::base::KeyValueConfiguration &m_kvc;
                    PlayerWidget *m_playerWidget;
            };

        }
    }
}

#endif /* COCKPIT_PLUGINS_PLAYERPLUGIN_H_ */

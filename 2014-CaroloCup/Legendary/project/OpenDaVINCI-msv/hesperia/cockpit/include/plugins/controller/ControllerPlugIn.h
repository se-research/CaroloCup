/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTROLLER_H_
#define CONTROLLER_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "plugins/ControlPlugIn.h"
#include "plugins/controller/ControllerWidget.h"

namespace cockpit {

    namespace plugins {

        namespace controller {

          class ControllerPlugIn : public ControlPlugIn {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ControllerPlugIn(const ControllerPlugIn &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ControllerPlugIn& operator=(const ControllerPlugIn &/*obj*/);

            public:
                /**
                 * Constructor.
                 *
                 * @param name Name of this plugin.
                 * @param kvc KeyValueConfiguration for this based widget.
                 * @param conf Client conference to send data to.
                 * @param prnt Pointer to the containing super window.
                 */
                ControllerPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, core::io::ContainerConference &conf, QWidget *prnt);

                virtual ~ControllerPlugIn();

                virtual QWidget* getQWidget() const;

                virtual void setupPlugin();

                virtual void stopPlugin();

            private:
                ControllerWidget *m_controllerWidget;
            };

        }
    }
}

#endif /*CONTROLLER_H_*/


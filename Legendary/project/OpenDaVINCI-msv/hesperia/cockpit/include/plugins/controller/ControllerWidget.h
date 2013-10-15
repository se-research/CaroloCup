/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTROLLERWIDGET_H_
#define CONTROLLERWIDGET_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/base/KeyValueConfiguration.h"
#include "core/base/Mutex.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/io/ContainerConference.h"
#include "core/io/ContainerListener.h"
#include "core/data/control/VehicleControl.h"

#include "QtIncludes.h"

#include "UserButtonData.h"

#include "plugins/PlugIn.h"

namespace cockpit {

    namespace plugins {

      namespace controller {

          using namespace std;

            /**
             * This class is the container for the controller widget.
             */
            class ControllerWidget : public QWidget, public core::io::ContainerListener {

                Q_OBJECT

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ControllerWidget(const ControllerWidget &/*obj*/);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ControllerWidget& operator=(const ControllerWidget &/*obj*/);

                public:
                    /**
                     * Constructor.
                     *
                     * @param plugIn Reference to the plugin to which this widget belongs.
                     * @param kvc KeyValueConfiguration for this based widget.
                     * @param conf Client conference to send data to.
                     * @param prnt Pointer to the parental widget.
                     */
                    ControllerWidget(const PlugIn &plugIn, const core::base::KeyValueConfiguration &kvc, core::io::ContainerConference &conf, QWidget *prnt);

                    virtual ~ControllerWidget();

                    virtual void nextContainer(core::data::Container &c);

                protected:
                    void keyPressEvent(QKeyEvent *event);

                public slots:
                    void TimerEvent();
                    void setHz(int v);

                    void userButtonPressed();
                    void userButtonReleased();

                    void sendButtonReleased();

                private:
                    core::io::ContainerConference &m_conference;
                    core::base::Mutex m_vehicleControlMutex;
                    core::data::control::VehicleControl m_vehicleControl;

                    core::base::Mutex m_HzMutex;
                    uint32_t m_Hz;
                    uint32_t m_counter;

                    QLabel *m_value;
                    QCheckBox *m_brakeLEDs;
                    QCheckBox *m_leftTurningLEDs;
                    QCheckBox *m_rightTurningLEDs;

                    core::base::Mutex m_userButtonMutex;
                    QPushButton *m_userButton;
                    core::data::TimeStamp m_userButtonPressedTS;
                    bool m_userButtonPressed;
                    msv::UserButtonData m_userButtonData;

                    core::base::Mutex m_sendVehicleControlDataMutex;
                    bool m_sendVehicleControlData;
                    QPushButton *m_sendVehicleControlButton;
            };
        }
    }
}

#endif /*CONTROLLERWIDGET_H_*/


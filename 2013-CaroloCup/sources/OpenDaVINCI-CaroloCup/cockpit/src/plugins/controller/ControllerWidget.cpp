/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <sstream>

#include "core/base/Lock.h"
#include "core/data/Container.h"

#include "QtIncludes.h"

#include "plugins/controller/ControllerWidget.h"

namespace cockpit {

    namespace plugins {

        namespace controller {

            using namespace std;
            using namespace core::base;
            using namespace core::io;
            using namespace core::data;
            using namespace core::data::control;

            ControllerWidget::ControllerWidget(const PlugIn &/*plugIn*/, const core::base::KeyValueConfiguration &/*kvc*/, ContainerConference &conf, QWidget *prnt) :
                QWidget(prnt),
                m_conference(conf),
                m_forceControlMutex(),
                m_forceControl(),
                m_HzMutex(),
                m_Hz(10),
                m_counter(0),
                m_value(NULL),
                m_brakeLEDs(NULL),
                m_leftTurningLEDs(NULL),
                m_rightTurningLEDs(NULL) {

                // Set size.
                setMinimumSize(640, 480);

                // Set frequency.
                QGroupBox *frequencyGroup = new QGroupBox(tr("Runtime frequency"));
                frequencyGroup->setFlat(false);
                QLabel *runtimeFrequencyLabel = new QLabel(tr("Runtime frequency [Hz]: "), this);
                QSpinBox *frequencySelector = new QSpinBox(this);
                frequencySelector->setMinimum(1);
                frequencySelector->setMaximum(20);
                frequencySelector->setValue(10);
                connect(frequencySelector, SIGNAL(valueChanged(int)), this, SLOT(setHz(int)));// Changing the value of spinbox will change the position of dial

                QHBoxLayout *frequencyLayout = new QHBoxLayout(this);
                frequencyLayout->addWidget(runtimeFrequencyLabel);
                frequencyLayout->addWidget(frequencySelector);
                frequencyGroup->setLayout(frequencyLayout);

                // Binary LED lights selectors.
                QGroupBox *LEDGroup = new QGroupBox(tr("Vehicle's LEDs"));
                LEDGroup->setFlat(false);
                m_brakeLEDs = new QCheckBox(tr("Brake LEDs"));
                m_leftTurningLEDs = new QCheckBox(tr("Left flashing LEDs"));
                m_rightTurningLEDs = new QCheckBox(tr("Right flashing LEDs"));

                QVBoxLayout *LEDselectorLayout = new QVBoxLayout();
                LEDselectorLayout->addWidget(m_brakeLEDs);
                LEDselectorLayout->addWidget(m_leftTurningLEDs);
                LEDselectorLayout->addWidget(m_rightTurningLEDs);
                LEDGroup->setLayout(LEDselectorLayout);

                // Combine frequency and LED status.
                QHBoxLayout *frequency_LED = new QHBoxLayout();
                frequency_LED->addWidget(frequencyGroup);
                frequency_LED->addWidget(LEDGroup);

                // ForceControl text.
                QGroupBox *ControlGroup = new QGroupBox(tr("Vehicle control (you must click in this area to control by keyboard!)"));
                QVBoxLayout *forceControlLayout = new QVBoxLayout();

                QLabel *description = new QLabel(tr("w=increase brake, d=decrease brake, up=accel., down=decel., left, right"));
                forceControlLayout->addWidget(description);
                m_value = new QLabel(m_forceControl.toString().c_str());
                forceControlLayout->addWidget(m_value);

                ControlGroup->setLayout(forceControlLayout);

                // Combine frequency_LED and ForceControl text.
                QVBoxLayout *frequency_LED_fc = new QVBoxLayout();
                frequency_LED_fc->addLayout(frequency_LED);
                frequency_LED_fc->addWidget(ControlGroup);

                setLayout(frequency_LED_fc);

                // Enforce the user to click into the window to control the vehicle.
                setFocusPolicy(Qt::StrongFocus);

                // Timer for sending data regularly.
                QTimer* timer = new QTimer(this);
                connect(timer, SIGNAL(timeout()), this, SLOT(TimerEvent()));
                timer->start(50);
            }

            ControllerWidget::~ControllerWidget() {}

            void ControllerWidget::nextContainer(Container &container) {
                if (container.getDataType() == Container::FORCECONTROL) {
                    ForceControl fc = container.getData<ForceControl>();
                    m_value->setText(fc.toString().c_str());
                }
            }

            void ControllerWidget::TimerEvent() {
                Lock l(m_HzMutex);
                
                if (m_counter == (20/m_Hz) ) {
                    Lock l2(m_forceControlMutex);

                    m_forceControl.setBrakeLights(m_brakeLEDs->isChecked());
                    m_forceControl.setLeftFlashingLights(m_leftTurningLEDs->isChecked());
                    m_forceControl.setRightFlashingLights(m_rightTurningLEDs->isChecked());

                    Container c(Container::FORCECONTROL, m_forceControl);
                    m_conference.send(c);
                    m_counter = 0;
                }

                if (m_counter > (20/m_Hz)) {
                    m_counter = 0;
                }

                m_counter++;
            }

            void ControllerWidget::setHz(int hz) {
                Lock l(m_HzMutex);
                if ( (hz > 0) && (hz < 21) ) {
                    m_Hz = hz;
                }
            }

            void ControllerWidget::keyPressEvent(QKeyEvent *evt) {
                switch(evt->key()){
                    case Qt::Key_Up:
                        {
                            Lock l2(m_forceControlMutex);
                            m_forceControl.setAccelerationForce(m_forceControl.getAccelerationForce() + 0.1);
                            break;
                        }
                    case Qt::Key_Left:
                        {
                            Lock l2(m_forceControlMutex);
                            m_forceControl.setSteeringForce(m_forceControl.getSteeringForce() - 0.1);
                            break;
                        }
                    case Qt::Key_Right:
                        {
                            Lock l2(m_forceControlMutex);
                            m_forceControl.setSteeringForce(m_forceControl.getSteeringForce() + 0.1);
                            break;
                        }
                    case Qt::Key_Down:
                        {
                            Lock l2(m_forceControlMutex);
                            m_forceControl.setAccelerationForce(m_forceControl.getAccelerationForce() - 0.1);
                            break;
                        }
                    case Qt::Key_W:
                        {
                            Lock l2(m_forceControlMutex);
                            m_forceControl.setBrakeForce(m_forceControl.getBrakeForce() + 0.1);
                            break;
                        }
                    case Qt::Key_S:
                        {
                            Lock l2(m_forceControlMutex);
                            m_forceControl.setBrakeForce(m_forceControl.getBrakeForce() - 0.1);
                            break;
                        }
                }
            }
        }
    }
}


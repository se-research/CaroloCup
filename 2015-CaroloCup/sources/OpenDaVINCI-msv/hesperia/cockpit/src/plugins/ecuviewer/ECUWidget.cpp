/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <sstream>
#include <cstring>

#include "STM32F4Control.h"
#include "STM32F4Data.h"

#include "plugins/ecuviewer/ECUWidget.h"

namespace cockpit {

    namespace plugins {

        namespace ecuviewer {

            using namespace std;
            using namespace core::data;
            using namespace core::io; 

            ECUWidget::ECUWidget(const PlugIn &/*plugIn*/, ContainerConference &conf, QWidget *prnt) :
                QWidget(prnt),
                m_conference(conf),
                m_rawDataFromECU(NULL),
                m_accelX(NULL),
                m_accelY(NULL),
                m_accelZ(NULL),
                m_firstCycle(true) {
                // Set size.
                setMinimumSize(640, 480);

                // Layout manager.
                QVBoxLayout* mainBox = new QVBoxLayout(this);

                //ListView and header construction
                m_rawDataFromECU = new QTextEdit(this);
                m_rawDataFromECU->setReadOnly(true);

                QFont courierFont("Courier", 12, QFont::Normal, false);
                m_rawDataFromECU->setFont(courierFont);

                QSpinBox *dataSelector = new QSpinBox(this);
                dataSelector->setMinimum(0);
                dataSelector->setMaximum(15);
                dataSelector->setValue(0);
                connect(dataSelector, SIGNAL(valueChanged(int)), this, SLOT(setDesiredData(int)));// Changing the value of spinbox will change the position of dial

                m_accelX = new QLabel(tr("0"), this);
                m_accelY = new QLabel(tr("0"), this);
                m_accelZ = new QLabel(tr("0"), this);

                QHBoxLayout *accelLayout = new QHBoxLayout(this);
                accelLayout->addWidget(m_accelX);
                accelLayout->addWidget(m_accelY);
                accelLayout->addWidget(m_accelZ);

                //add to Layout
                mainBox->addLayout(accelLayout);
                mainBox->addWidget(m_rawDataFromECU);

                // Set layout manager.
                setLayout(mainBox);
            }

            ECUWidget::~ECUWidget() {
                OPENDAVINCI_CORE_DELETE_POINTER(m_rawDataFromECU);
                OPENDAVINCI_CORE_DELETE_POINTER(m_accelX);
                OPENDAVINCI_CORE_DELETE_POINTER(m_accelY);
                OPENDAVINCI_CORE_DELETE_POINTER(m_accelZ);
            }

            void ECUWidget::nextContainer(Container &container) {
                if (container.getDataType() == Container::USER_DATA_0) {
                    msv::STM32F4Data d = container.getData<msv::STM32F4Data>();
                    m_rawDataFromECU->append(QString(d.toString().c_str()));
                    m_rawDataFromECU->verticalScrollBar()->setValue(m_rawDataFromECU->verticalScrollBar()->maximum());
                }
            }

            void ECUWidget::setDesiredData(int v) {
                if (!m_firstCycle) {
                    msv::STM32F4Control feed;
                    feed.setDataFeed(v);

                    Container c(Container::USER_DATA_1, feed);

                    m_conference.send(c);
                }
                m_firstCycle = false;
            }

        } 
    }
}

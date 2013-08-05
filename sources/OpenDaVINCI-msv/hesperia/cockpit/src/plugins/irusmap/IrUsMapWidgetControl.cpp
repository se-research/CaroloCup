/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <algorithm>

#include "QtIncludes.h"

#include "core/macros.h"
#include "core/base/Lock.h"
#include "core/data/Container.h"

#include "plugins/irusmap/IrUsMapWidget.h"
#include "plugins/irusmap/IrUsMapWidgetControl.h"

namespace cockpit {

    namespace plugins {

        namespace irusmap {

            using namespace std;
            using namespace core::base;
            using namespace core::data;

            IrUsMapWidgetControl::IrUsMapWidgetControl(const PlugIn &plugIn, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
                QWidget(prnt),
                m_mapWidgetMutex(),
                m_mapWidget(NULL) {

                {
                    Lock l(m_mapWidgetMutex);
                    m_mapWidget = new IrUsMapWidget(plugIn, kvc, prnt);
                }

                QLabel *scaleLabel = new QLabel(tr("Scale: "), this);
                QSpinBox *scaleSelector = new QSpinBox(this);
                scaleSelector->setMinimum(1);
                scaleSelector->setMaximum(10);
                scaleSelector->setValue(5);
                connect(scaleSelector, SIGNAL(valueChanged(int)), this, SLOT(setScale(int)));

                QHBoxLayout *scaleLayout = new QHBoxLayout();
                scaleLayout->addWidget(scaleLabel);
                scaleLayout->addWidget(scaleSelector);

                QVBoxLayout *mainLayout = new QVBoxLayout(this);
                mainLayout->addLayout(scaleLayout);
                mainLayout->addWidget(m_mapWidget);

                setLayout(mainLayout);

                // Set size.
                setMinimumSize(640, 480);
            }

            IrUsMapWidgetControl::~IrUsMapWidgetControl() {
                Lock l(m_mapWidgetMutex);
                m_mapWidget->stopTimer();
                OPENDAVINCI_CORE_DELETE_POINTER(m_mapWidget);
            }

            void IrUsMapWidgetControl::setScale(int val) {
                Lock l(m_mapWidgetMutex);
                if (m_mapWidget != NULL) {
                    m_mapWidget->setScale(val);
                }
            }

            void IrUsMapWidgetControl::nextContainer(Container &c) {
                Lock l(m_mapWidgetMutex);
                if (m_mapWidget != NULL) {
                    m_mapWidget->nextContainer(c);
                }
            }

        }
    }
}


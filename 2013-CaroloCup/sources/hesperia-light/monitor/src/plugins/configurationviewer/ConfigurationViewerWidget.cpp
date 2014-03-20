/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>

#include "plugins/configurationviewer/ConfigurationViewerWidget.h"

namespace plugins {
    namespace configurationviewer {

        using namespace std;

        ConfigurationViewerWidget::ConfigurationViewerWidget(const PlugIn &/*plugIn*/, const core::base::KeyValueConfiguration &kvc, QWidget *prnt) :
            QWidget(prnt),
            m_configurationDataText(NULL) {

            // Set size.
            setMinimumSize(640, 480);

            // Layout manager.
            QGridLayout *mainBox = new QGridLayout(this);

            // QLabel to show configuration data.
            m_configurationDataText = new QTextEdit(this);

            QFont courierFont("Courier", 12, QFont::Normal, false);
            m_configurationDataText->setFont(courierFont);

            // Get textual representation.
            stringstream configurationText;
            configurationText << kvc;

            m_configurationDataText->append(QString(configurationText.str().c_str()));
            m_configurationDataText->setReadOnly(true);

            mainBox->addWidget(m_configurationDataText, 0, 0);

            // Set layout manager.
            setLayout(mainBox);
        }

        ConfigurationViewerWidget::~ConfigurationViewerWidget() {
            HESPERIA_CORE_DELETE_POINTER(m_configurationDataText);
        }

    }
}

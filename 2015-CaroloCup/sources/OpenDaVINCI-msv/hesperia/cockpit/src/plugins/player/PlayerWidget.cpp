/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include <sstream>
#include <string>

#include "core/macros.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/exceptions/Exceptions.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"

#include "plugins/player/PlayerWidget.h"

namespace cockpit {

    namespace plugins {

        namespace player {

            using namespace std;
            using namespace core::data;
            using namespace tools::player;

            PlayerWidget::PlayerWidget(const PlugIn &/*plugIn*/, const core::base::KeyValueConfiguration &kvc, core::io::ContainerConference &conf, QWidget *prnt) :
                QWidget(prnt),
                m_kvc(kvc),
                m_conference(conf),
                m_playBtn(NULL),
                m_pauseBtn(NULL),
                m_rewindBtn(NULL),
                m_stepBtn(NULL),
                m_timeScale(NULL),
                m_autoRewind(NULL),
                m_desc(NULL),
                m_timeScaleFactor(1),
                m_player(NULL) {
                // Set size.
                setMinimumSize(400, 150);

                // Button control.
                QPushButton *loadFileBtn = new QPushButton("Load recording", this);
                QObject::connect(loadFileBtn, SIGNAL(clicked()), this, SLOT(loadFile()));

                QHBoxLayout *fileOperations = new QHBoxLayout();
                fileOperations->addWidget(loadFileBtn);

                m_desc = new QLabel("No file loaded.");

                // Play/pause control.
                m_playBtn = new QPushButton("Play", this);
                m_playBtn->setEnabled(false);
                QObject::connect(m_playBtn, SIGNAL(clicked()), this, SLOT(play()));

                m_pauseBtn = new QPushButton("Pause", this);
                m_pauseBtn->setEnabled(false);
                QObject::connect(m_pauseBtn, SIGNAL(clicked()), this, SLOT(pause()));

                m_rewindBtn = new QPushButton("Rewind", this);
                m_rewindBtn->setEnabled(false);
                QObject::connect(m_rewindBtn, SIGNAL(clicked()), this, SLOT(rewind()));

                m_stepBtn = new QPushButton("Step", this);
                m_stepBtn->setEnabled(false);
                QObject::connect(m_stepBtn, SIGNAL(clicked()), this, SLOT(step()));

                // Replay speed
                QLabel *lblTimeScale = new QLabel(tr("Time scale:"));
                m_timeScale = new QSpinBox(this);
                m_timeScale->setRange(1, 10);
                m_timeScale->setSingleStep(1);
                m_timeScale->setSuffix("x");
                m_timeScale->setValue(1);

                connect(m_timeScale, SIGNAL(valueChanged(int)), this, SLOT(changeTimeScale(int)));

                QHBoxLayout *timeScale = new QHBoxLayout();
                timeScale->addWidget(lblTimeScale);
                timeScale->addWidget(m_timeScale);

                m_autoRewind = new QCheckBox("Auto rewind", this);

                QHBoxLayout *operations = new QHBoxLayout();
                operations->addWidget(m_playBtn);
                operations->addWidget(m_pauseBtn);
                operations->addWidget(m_rewindBtn);
                operations->addWidget(m_stepBtn);
                operations->addLayout(timeScale);
                operations->addWidget(m_autoRewind);

                // Final layout.
                QVBoxLayout *mainLayout = new QVBoxLayout(this);
                mainLayout->addLayout(fileOperations);
                mainLayout->addWidget(m_desc);
                mainLayout->addLayout(operations);

                setLayout(mainLayout);
            }

            PlayerWidget::~PlayerWidget() {
                OPENDAVINCI_CORE_DELETE_POINTER(m_player);
            }

            void PlayerWidget::play() {
                m_playBtn->setEnabled(false);
                m_pauseBtn->setEnabled(true);
                m_rewindBtn->setEnabled(false);
                m_stepBtn->setEnabled(false);

                sendNextContainer();
            }

            void PlayerWidget::pause() {
                m_playBtn->setEnabled(true);
                m_pauseBtn->setEnabled(false);
                m_stepBtn->setEnabled(true);
                m_rewindBtn->setEnabled(true);
            }

            void PlayerWidget::rewind() {
                if (m_player != NULL) {
                    m_player->rewind();
                }
                m_playBtn->setEnabled(true);
                m_pauseBtn->setEnabled(false);
                m_stepBtn->setEnabled(true);
                m_rewindBtn->setEnabled(false);
            }

            void PlayerWidget::step() {
                sendNextContainer();
            }

            void PlayerWidget::changeTimeScale(int v) {
                if (v > 0) {
                    m_timeScaleFactor = 1.0/v;
                }
            }

            void PlayerWidget::sendNextContainer() {
                if (m_player != NULL) {
                    if (!m_player->hasMoreData() && m_autoRewind->isChecked()) {
                        m_player->rewind();
                    }

                    // Get container to be sent.
                    Container nextContainerToBeSent = m_player->getNextContainerToBeSent();

                    // Get delay to wait _after_ sending the container.
                    uint32_t delay = m_player->getDelay() / 1000;

                    // Send container.
                    if ( (nextContainerToBeSent.getDataType() != Container::UNDEFINEDDATA) &&
                            (nextContainerToBeSent.getDataType() != Container::PLAYER_COMMAND) ) {
                        m_conference.send(nextContainerToBeSent);
                    }

                    // Continously playing if "pause" button is enabled.
                    if (m_pauseBtn->isEnabled()) {
                        QTimer::singleShot(delay * m_timeScaleFactor, this, SLOT(sendNextContainer()));
                    }
                }
            }

            void PlayerWidget::loadFile() {
                string fn = QFileDialog::getOpenFileName(this, tr("Open previous recording file"), "", tr("Recording files (*.rec)")).toStdString();

                if (!fn.empty()) {
                    OPENDAVINCI_CORE_DELETE_POINTER(m_player);

                    stringstream s;
                    s << "file://" << fn;
                    core::io::URL url(s.str());

                    m_desc->setText(url.toString().c_str());

                    // Size of the memory buffer.
                    const uint32_t MEMORY_SEGMENT_SIZE = m_kvc.getValue<uint32_t>("global.buffer.memorySegmentSize");

                    // Number of memory segments.
                    const uint32_t NUMBER_OF_SEGMENTS = m_kvc.getValue<uint32_t>("global.buffer.numberOfMemorySegments");

                    const bool AUTO_REWIND = false;
                    m_player = new Player(url, AUTO_REWIND, MEMORY_SEGMENT_SIZE, NUMBER_OF_SEGMENTS);

                    m_playBtn->setEnabled(true);
                    m_pauseBtn->setEnabled(false);
                    m_stepBtn->setEnabled(true);
                    m_rewindBtn->setEnabled(false);
                }
            }

        }
    }
}

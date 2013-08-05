/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <cmath>
#include <iostream>
#include <queue>

#include "core/macros.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"

#include "core/data/player/PlayerCommand.h"

#include "Player.h"
#include "PlayerCache.h"

namespace player {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;

    Player::Player(const int32_t &argc, char **argv) :
            ConferenceClientModule(argc, argv, "Player"),
            m_playerControl() {}

    Player::~Player() {}

    void Player::setUp() {}

    void Player::tearDown() {}

    void Player::wait() {
        AbstractModule::wait();
    }

    ModuleState::MODULE_EXITCODE Player::body() {
        // Check if the player is remotely controlled.
        bool remoteControl = (getKeyValueConfiguration().getValue<bool>("player.remoteControl") != 0);

        // Read the URL of the file to replay.
        URL url(getKeyValueConfiguration().getValue<string>("player.input"));

        // Read the scaling factor.
        double timeScale = getKeyValueConfiguration().getValue<double>("player.timeScale");

        // Do we have to rewind the stream on EOF?
        bool autoRewind = (getKeyValueConfiguration().getValue<int>("player.autoRewind") != 0);

        // Size of the cache.
        uint32_t sizeOfCache = (getKeyValueConfiguration().getValue<uint32_t>("player.sizeOfCache") != 0);

        // Get the stream using the StreamFactory with the given URL.
        istream &in = StreamFactory::getInstance().getInputStream(url);

        // Setup cache.
        PlayerCache cache(sizeOfCache, autoRewind, in);
        cache.start();

        // TODO: Check for corrupt player files.
        while (cache.getNumberOfEntries() < 1) {
            Thread::usleep(1000);
        }

        // Add FIFOQueue for controlling the player.
        addDataStoreFor(Container::PLAYER_COMMAND, m_playerControl);

        // The "actual" container contains the data to be send, ...
        Container actual;
        // ... whereas the "successor" container contains the data that follows the actual one.
        Container successor;

        // This flag indicates if new data has to be read from the stream.
        bool successorProcessed = true;

        // This flag indicates, that we have to seek to the beginning of the stream and read the "actual" container.
        bool seekToTheBeginning = true;

        // Delay between two containers.
        uint32_t delay = 0;

        // If no remote control, simply play the stuff.
        bool playing = (!remoteControl);
        bool doStep = false;

        // The main loop.
        while (getModuleState() == ModuleState::RUNNING) {
            if (playing) {
                // Check, if we are "at the beginning".
                if (seekToTheBeginning) {
                    // Read the "actual" (first) container.
                    if (cache.getNumberOfEntries() > 0) {
                        actual = cache.getNextContainer();
                    }

                    // Disable this state.
                    seekToTheBeginning = false;

                    // Don't skip reading successor.
                    successorProcessed = true;
                }

                // While there are more container, read the "successor" of the "actual" container.
                if (successorProcessed) {
                    if (cache.getNumberOfEntries() > 0) {
                        successor = cache.getNextContainer();

                        if (successor.getDataType() != Container::UNDEFINEDDATA) {
                            // Indicate that the successor needs to be processed.
                            successorProcessed = false;
                        } else {
                            // If the container contains undefined data and autorewind is chosen, seek to the beginning.
                            if (autoRewind) {
                                // Change internal state to "at the beginning".
                                seekToTheBeginning = true;
                            }
                        }
                    }
                }

                // Here, the time gap between two containers is computed.
                if ( (actual.getDataType() != Container::UNDEFINEDDATA) &&
                        (successor.getDataType() != Container::UNDEFINEDDATA) ) {
                    TimeStamp delta = successor.getReceivedTimeStamp() - actual.getReceivedTimeStamp();
                    if (delta.toMicroseconds() > 0) {
                        if (fabs(timeScale) > 1e-5) {
                            delay = (long)((double)delta.toMicroseconds() * fabs(timeScale));
                        }
                    }
                }

                // Here, the "actual" container is sent while discarding player commands.
                // TODO: Filter data.
                if ( (actual.getDataType() != Container::UNDEFINEDDATA) &&
                        (actual.getDataType() != Container::PLAYER_COMMAND) ) {

                    // Don't wait while stepping.
                    if (!doStep) {
                        Thread::usleep(delay);
                    }

                    // Process next token only if there's no new command.
                    if (!remoteControl || (m_playerControl.isEmpty())) {
                        cerr << "SENT (delay was " << delay << " us): " << actual.getReceivedTimeStamp().toString() << endl;
                        getConference().send(actual);

                        // Process the "successor" container as the next "actual" one.
                        actual = successor;

                        // The actual container has been sent, therefore, the successor becomes the next actual container.
                        successorProcessed = true;
                    }
                }
            }

            // If the last cycle was during a step, disable the stepwise playing and check for remote control.
            if (doStep) {
                playing = false;
                doStep = false;
            }

            // Check remote control.
            if (remoteControl) {
                if (!(m_playerControl.isEmpty())) {
                    Container container = m_playerControl.leave();

                    core::data::player::PlayerCommand pc;
                    pc = container.getData<core::data::player::PlayerCommand>();

                    cerr << "Player: " << pc.toString() << endl;
                    switch (pc.getCommand()) {
                    case core::data::player::PlayerCommand::PLAY:
                        playing = true;
                        break;
                    case core::data::player::PlayerCommand::PAUSE:
                        playing = false;
                        break;
                    case core::data::player::PlayerCommand::STEP_FORWARD:
                        playing = true;
                        doStep = true;
                        break;
                    case core::data::player::PlayerCommand::REWIND:
                        // Clear cache and wait for new data.
                        cache.clearQueueRewindInputStream();
                        while (cache.getNumberOfEntries() < 1) {
                            Thread::usleep(1000);
                        }
                        seekToTheBeginning = true;
                        playing = false;
                        break;
                    }
                }
            }
        }

        cache.stop();

        return ModuleState::OKAY;
    }

} // player

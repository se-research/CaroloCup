/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <sstream>

#include "core/base/Thread.h"
#include "core/data/Container.h"

#include "tools/player/Player.h"
#include "tools/recorder/Recorder.h"
#include "tools/splitter/Splitter.h"


namespace tools {
    namespace splitter {

        using namespace std;
        using namespace core::base;
        using namespace core::data;
        using namespace tools::player;
        using namespace tools::recorder;

        Splitter::Splitter() {}

        Splitter::~Splitter() {}

        void Splitter::process(const string &source, const uint32_t &memorySegmentSize, const uint32_t &start, const uint32_t &end) {
            // Run player and recorder in synchronous mode.
            const bool THREADING = false;

            // Number of memory segments can be set to a fixed value.
            const uint32_t NUMBER_OF_SEGMENTS = 3;

            // Compose URL for source.
            stringstream playbackURL;
            playbackURL << "file://" << source;

            // Stop playback at EOF.
            const bool AUTO_REWIND = false;

            // Construct player.
            Player player(playbackURL.str(), AUTO_REWIND, memorySegmentSize, NUMBER_OF_SEGMENTS, THREADING);

            // Compose URL for storing containers.
            stringstream recordingURL;
            recordingURL << "file://" << source << "_" << start << "-" << end << ".rec";

            // Construct recorder.
            Recorder recorder(recordingURL.str(), memorySegmentSize, NUMBER_OF_SEGMENTS, THREADING);

            // The next container to be sent.
            Container nextContainerToBeSent;

            uint32_t containerCounter = 0;

            // The main processing loop.
            while (player.hasMoreData() && (containerCounter <= end)) {
                // Get container to be sent.
                nextContainerToBeSent = player.getNextContainerToBeSent();

                if (containerCounter >= start && containerCounter <= end) {
                    cout << "Processing container " << containerCounter << " of type '" << nextContainerToBeSent.toString() << "'";
                    recorder.store(nextContainerToBeSent);
                    cout << "." << endl;
                }

                containerCounter++;
            }
        }

    } // splitter
} // tools

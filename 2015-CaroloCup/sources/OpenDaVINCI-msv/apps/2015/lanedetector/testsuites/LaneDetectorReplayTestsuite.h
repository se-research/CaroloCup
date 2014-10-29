/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef LANEDETECTORREPLAYTESTSUITE_H_
#define LANEDETECTORREPLAYTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <string>

#include "context/base/DirectInterface.h"
#include "context/base/RecordingContainer.h"
#include "context/base/RuntimeControl.h"
#include "context/base/RuntimeEnvironment.h"
#include "context/base/PlaybackContainer.h"
#include "vehiclecontext/VehicleRuntimeControl.h"

#include "../include/LaneDetector.h"

using namespace std;
using namespace context::base;
using namespace vehiclecontext;
using namespace core::data;
using namespace core::data::image;
using namespace msv;

class LaneDetectorReplayTestSuite : public CxxTest::TestSuite {
    public:

        void testReplayRecording() {
            const uint32_t MEMORYSEGMENT_SIZE = 2800000;
            const uint32_t NUMBER_OF_MEMORYSEGMENTS = 20;
            const float FREQ = 10;

            // 0. Setup system's configuration.
            stringstream sstrConfiguration;

            // TODO: Check relative path for scenario.
            sstrConfiguration
                  << "# The following attributes define the buffer sizes for recording and replaying." << endl
                  << "# You need to adjust these parameters depending on the camera resolution for example (640x480x3 --> 1000000 for memorySegment, 1280x720x3 --> 2800000)." << endl
                  << "global.buffer.memorySegmentSize = " << MEMORYSEGMENT_SIZE << " # Size of a memory segment for a shared data stream in bytes." << endl
                  << "global.buffer.numberOfMemorySegments = " << NUMBER_OF_MEMORYSEGMENTS << " # Number of memory segments used for buffering." << endl
                  << endl
                  << "#" << endl
                  << "# CONFIGURATION FOR LANEDETECTOR" << endl
                  << "#" << endl
                  << "lanedetector.camera_id = 2  # select here the proper camera" << endl
                  << "lanedetector.debug = 1      # set to 0 to disable any windows and further output" << endl
                  << endl;

            // 1. Setup runtime control.
            DirectInterface di("225.0.0.100", 100, sstrConfiguration.str());
            VehicleRuntimeControl vrc(di);
            vrc.setup(RuntimeControl::TAKE_CONTROL);

            // 2. Setup application.
            string argv0("lanedetector");
            string argv1("--cid=100");
            string argv2("--freq=10");
            string argv3("--verbose=1");
            int32_t argc = 4;
            char **argv;
            argv = new char*[4];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());
            argv[2] = const_cast<char*>(argv2.c_str());
            argv[3] = const_cast<char*>(argv3.c_str());

            // 3. Instantiate actual System-Under-Test.
            LaneDetector myLaneDetector(argc, argv);

            // 4. Instantiate replay.
            PlaybackContainer pbc(FREQ, "file://recorder.rec", MEMORYSEGMENT_SIZE, NUMBER_OF_MEMORYSEGMENTS);

            // 5. Compose simulation of system's context.
            RuntimeEnvironment rte;
            rte.add(myLaneDetector);
            rte.add(pbc);

            // 6. Run application under supervision of RuntimeControl for maximum 100s.
            TS_ASSERT(vrc.run(rte, 370) == RuntimeControl::APPLICATIONS_FINISHED);

            // 7. And finally clean up.
            vrc.tearDown();
        }

};

#endif // LANEDETECTORREPLAYTESTSUITE_H_

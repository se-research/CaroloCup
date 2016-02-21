/**
 * lanedetector-demo - Sample application run the lane-detector as
 *                     stand alone application.
 * Copyright (C) 2012 - 2015 Christian Berger
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#include <sstream>
#include <string>
#include <iostream>

#include "opendavinci/odcore/data/Container.h"
#include "opendavinci/odcore/io/URL.h"
#include "opendavinci/odcore/io/StreamFactory.h"
#include "opendavinci/odcore/data/TimeStamp.h"

#include "opendavinci/odcontext/base/DirectInterface.h"
#include "opendavinci/odcontext/base/RecordingContainer.h"
#include "opendavinci/odcontext/base/RuntimeControl.h"
#include "opendavinci/odcontext/base/RuntimeEnvironment.h"
#include "opendavinci/odcontext/base/PlaybackContainer.h"
#include "opendlv/vehiclecontext/VehicleRuntimeControl.h"

#include "LaneDetectionData.h"

#include "../include/LaneDetector.h"

using namespace std;
using namespace odcore::base;
using namespace odcore::data;
using namespace odcore::io;
using namespace odcontext::base;
using namespace opendlv::vehiclecontext;
using namespace msv;

int main() {
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
          << "lanedetector.what_to_inspect = 7" << endl
          << "lanedetector.showRes_getContours = 1" << endl
          << "lanedetector.showRes_getRectangles = 1" << endl
          << "lanedetector.showRes_classification = 1" << endl
          << "lanedetector.showRes_filterAndMerge = 1" << endl
          << "lanedetector.showRes_finalFilter = 1" << endl
          << "lanedetector.showRes_finalResult = 1" << endl
          << "lanedetector.showRes_createTrajectory = 1" << endl
          << "lanedetector.threshBaseParameter=48" << endl
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
    PlaybackContainer pbc(FREQ, "file://recording.rec", MEMORYSEGMENT_SIZE, NUMBER_OF_MEMORYSEGMENTS);

    // 5. Compose simulation of system's context.
    RuntimeEnvironment rte;
    rte.add(myLaneDetector);
    rte.add(pbc);

    // 6. Run application under supervision of RuntimeControl for maximum 100s.
    vrc.run(rte, 105);

    // 7. And finally clean up.
    vrc.tearDown();
}


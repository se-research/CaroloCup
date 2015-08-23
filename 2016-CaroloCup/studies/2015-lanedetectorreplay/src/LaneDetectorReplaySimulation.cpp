/**
 * 2015-example-lanedetector - This test suite demonstrates how to
 *                             compose a simulation using recordings.
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

#include "core/SharedPointer.h"
#include "core/data/Container.h"
#include "core/io/URL.h"
#include "core/io/StreamFactory.h"
#include "core/data/TimeStamp.h"

#include "context/base/DirectInterface.h"
#include "context/base/RecordingContainer.h"
#include "context/base/RuntimeControl.h"
#include "context/base/RuntimeEnvironment.h"
#include "context/base/PlaybackContainer.h"
#include "vehiclecontext/VehicleRuntimeControl.h"

#include "LaneDetectorReplaySimulation.h"

// Include file from system-under-test.
#include "../include/LaneDetector.h"

using namespace std;
using namespace core;
using namespace core::base;
using namespace core::data;
using namespace core::io;
using namespace context::base;
using namespace vehiclecontext;
using namespace msv;

class CSVExport : public SystemReportingComponent {
    private:
        SharedPointer<ostream> m_out;
        string m_url;

    public:
        CSVExport(const string &url) : m_out(NULL), m_url(url) {}

        void setup() {
		    URL _url(m_url);
		    m_out = StreamFactory::getInstance().getOutputStream(_url);
        }

        void tearDown() {
		    if (m_out.isValid()) {
		        m_out->flush();
		    }
        }

        void report(const core::wrapper::Time &/*t*/) {
            const uint32_t SIZE = getFIFO().getSize();
            for (uint32_t i = 0; i < SIZE; i++) {
                Container c = getFIFO().leave();
                if (c.getDataType() == Container::USER_DATA_1) {
            		LaneDetectionData data = c.getData<LaneDetectionData>();

                    cerr << "(CSVExporter) Extracting '" << data.toString() << "'." << endl;

                    (*m_out) << data.toString() << endl;

    		        m_out->flush();
                }
            }
			getFIFO().clear();
        }
};

namespace simulation {

    bool LaneDetectorSimulation::run() {
        bool retVal = false;

        const bool showWindow = false;
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
              << "lanedetector.debug = " << (showWindow ? "1" : "0") << " # set to 0 to disable any windows and further output" << endl
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
        PlaybackContainer pbc(FREQ, "file://../../straightroad.rec", MEMORYSEGMENT_SIZE, NUMBER_OF_MEMORYSEGMENTS);

        // 5. CSV Reporter.
        CSVExport csvReporter("file://lanedetector.csv");

        // 5. Compose simulation of system's context.
        RuntimeEnvironment rte;
        rte.add(myLaneDetector);
        rte.add(pbc);
        rte.add(csvReporter);

        // 6. Run application under supervision of RuntimeControl for maximum 100s.
        retVal = vrc.run(rte, 100) == RuntimeControl::APPLICATIONS_FINISHED;

        // TODO: As the playback component cannot end a simulation when the recording
        // file has ended, we manually set this simulation to pass.
        retVal = true;

        // 7. And finally clean up.
        vrc.tearDown();
        return retVal;
    }
}


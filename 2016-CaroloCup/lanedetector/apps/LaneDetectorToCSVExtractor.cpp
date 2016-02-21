/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <sstream>
#include <string>
#include <iostream>

#include "opendavinci/odcore/SharedPointer.h"
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

#include "LaneDetector.h"

using namespace std;
using namespace odcore;
using namespace odcore::base;
using namespace odcore::data;
using namespace odcore::io;
using namespace odcontext::base;
using namespace opendlv::vehiclecontext;
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
                if (c.getDataType() == LaneDetectionData::ID()) {
            		LaneDetectionData data = c.getData<LaneDetectionData>();

                    cerr << "(CSVExporter) Extracting '" << data.toString() << "'." << endl;

                    (*m_out) << data.toString() << endl;

    		        m_out->flush();
                }
            }
			getFIFO().clear();
        }

};

int32_t main(int32_t _argc, char **_argv) {
    if (_argc == 4) {
        const string recordingFile(_argv[1]);
        const string csvFile(_argv[2]);

        stringstream timeoutStr;
        timeoutStr << _argv[3];
        uint32_t timeOut = 0;
        timeoutStr >> timeOut;

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
        stringstream s1;
        s1 << "file://" << recordingFile;
        PlaybackContainer pbc(FREQ, s1.str(), MEMORYSEGMENT_SIZE, NUMBER_OF_MEMORYSEGMENTS);

        // 5. CSV Reporter.
        stringstream s2;
        s2 << "file://" << csvFile;
        CSVExport csvReporter(s2.str());

        // 5. Compose simulation of system's context.
        RuntimeEnvironment rte;
        rte.add(myLaneDetector);
        rte.add(pbc);
        rte.add(csvReporter);

        // 6. Run application under supervision of RuntimeControl for maximum 100s.
        vrc.run(rte, timeOut);

        // 7. And finally clean up.
        vrc.tearDown();
    }
    else {
        cerr << "Run with: " << _argv[0] << " myRecording.rec output.csv timeout" << endl;
        cerr << endl;
        cerr << "Example: " << _argv[0] << " recorder.rec out.csv 370" << endl;
    }

    return 0;
}


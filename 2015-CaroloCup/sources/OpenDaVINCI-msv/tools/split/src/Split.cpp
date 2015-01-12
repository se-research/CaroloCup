/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>
#include <vector>

#include "core/StringToolbox.h"
#include "core/data/Container.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"
#include "core/data/image/SharedImage.h"
#include "core/base/CommandLineParser.h"

#include "tools/player/Player.h"
#include "tools/recorder/Recorder.h"
#include "tools/splitter/Splitter.h"

#include "Split.h"

namespace split {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;
    using namespace tools::player;
    using namespace tools::recorder;
    using namespace tools::splitter;

    Split::Split(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "split"),
        m_source(),
        m_range() {
        // Parse command line arguments.
        parseAdditionalCommandLineParameters(argc, argv);
    }

    Split::~Split() {}

    void Split::setUp() {}

    void Split::tearDown() {}

    void Split::parseAdditionalCommandLineParameters(const int &argc, char **argv) {
        CommandLineParser cmdParser;
        cmdParser.addCommandLineArgument("source");
        cmdParser.addCommandLineArgument("range");

        cmdParser.parse(argc, argv);

        CommandLineArgument cmdArgumentSOURCE = cmdParser.getCommandLineArgument("source");
        CommandLineArgument cmdArgumentRANGE = cmdParser.getCommandLineArgument("range");

        if (cmdArgumentSOURCE.isSet()) {
            m_source = cmdArgumentSOURCE.getValue<string>();
            core::StringToolbox::trim(m_source);
        }

        if (cmdArgumentRANGE.isSet()) {
            m_range = cmdArgumentRANGE.getValue<string>();
            core::StringToolbox::trim(m_range);
        }
    }

    ModuleState::MODULE_EXITCODE Split::body() {
        ModuleState::MODULE_EXITCODE retVal = ModuleState::OKAY;

        // Size of the memory buffer.
        const uint32_t MEMORY_SEGMENT_SIZE = getKeyValueConfiguration().getValue<uint32_t>("global.buffer.memorySegmentSize");

        // Split the range parameter.
        vector<string> rangeTokens = core::StringToolbox::split(m_range, '-');

        if (rangeTokens.size() == 2) {
            uint32_t start = 0, end = 0;
            stringstream s_start;
            s_start << rangeTokens.at(0);
            s_start >> start;

            stringstream s_end;
            s_end << rangeTokens.at(1);
            s_end >> end;

            if (start < end) {
                Splitter s;
                s.process(m_source, MEMORY_SEGMENT_SIZE, start, end);
            }
            else {
                retVal = ModuleState::SERIOUS_ERROR;
            }
        }

        return retVal;
    }

} // split


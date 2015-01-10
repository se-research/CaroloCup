/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "core/data/Container.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"
#include "core/data/image/SharedImage.h"

#include "RecIntegrity.h"

namespace recintegrity {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;

    RecIntegrity::RecIntegrity(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "recintegrity")
    {}

    RecIntegrity::~RecIntegrity() {}

    void RecIntegrity::setUp() {}

    void RecIntegrity::tearDown() {}

    ModuleState::MODULE_EXITCODE RecIntegrity::body() {
        // Read the URL of the file to replay.
        URL url(getKeyValueConfiguration().getValue<string>("recintegrity.input"));

        // Get the stream using the StreamFactory with the given URL.
        istream &in = StreamFactory::getInstance().getInputStream(url);

        if (in.good()) {
            // Determine file size.
            in.seekg(0, in.end);
            int32_t length = in.tellg();
            in.seekg(0, in.beg);

            int32_t oldPercentage = -1;
            bool fileNotCorrupt = true;
            uint32_t numberOfSharedImages = 0;
            uint32_t numberOfSharedData = 0;
            while (in.good()) {
                Container c;
                in >> c;

                if (in.gcount() > 0) {
                    int32_t currPos = in.tellg();

                    fileNotCorrupt &= (c.getDataType() != Container::UNDEFINEDDATA) && (currPos > 0);

                    // If the data is from SHARED_IMAGE, skip the raw data from the shared memory segment.
                    if (c.getDataType() == Container::SHARED_IMAGE) {
                        core::data::image::SharedImage si = c.getData<core::data::image::SharedImage>();

                        uint32_t lengthToSkip = si.getSize();

                        in.seekg(currPos + lengthToSkip);
                        cout << "  Found SHARED_IMAGE '" << si.getName() << "' (" << lengthToSkip << " bytes)" << endl;
                        numberOfSharedImages++;
                    }
                    else if (c.getDataType() == Container::SHARED_DATA) {
                        core::data::SharedData sd = c.getData<core::data::SharedData>();

                        uint32_t lengthToSkip = sd.getSize();

                        in.seekg(currPos + lengthToSkip);
                        cout << "  Found SHARED_DATA '" << sd.getName() << "' (" << lengthToSkip << " bytes)" << endl;
                        numberOfSharedData++;
                    }

                    float percentage = (float)(currPos*100.0)/(float)length;

                    if ( ((int32_t)percentage % 5 == 0) && ((int32_t)percentage != oldPercentage) ) {
                        cout << percentage << "% (" << currPos << "/" << length << " bytes processed)." << endl;
                        oldPercentage = (int32_t)percentage;
                    }
                }
            }
            cout << "Input file is " << ((fileNotCorrupt) ? "not " : "") << "corrupt, contains " << numberOfSharedImages << " shared images and " << numberOfSharedData << " shared data segments." << endl;
        }

        return ModuleState::OKAY;
    }

} // recintegrity


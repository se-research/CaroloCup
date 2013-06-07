/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <unistd.h>

#include "core/macros.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/Lock.h"
#include "core/base/Mutex.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/io/URL.h"
#include "core/io/StreamFactory.h"

#include "Clock.h"
#include "Rec2Video.h"

namespace rec2video {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;

    Rec2Video::Rec2Video(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "Rec2Video"),
        m_conditionRenderer(),
        m_doRendering(false),
        m_renderer(NULL) {}

    Rec2Video::~Rec2Video() {
        HESPERIA_CORE_DELETE_POINTER(m_renderer);
    }

    void Rec2Video::setUp() {}

    void Rec2Video::tearDown() {}

    ModuleState::MODULE_EXITCODE Rec2Video::body() {
        const KeyValueConfiguration kvc = getKeyValueConfiguration();
        m_renderer = new Renderer(kvc, m_conditionRenderer, m_doRendering);
        m_renderer->start();

        // Read the URL of the file to replay.
        URL url(kvc.getValue<string>("rec2video.input"));

        // Get the stream using the StreamFactory with the given URL.
        istream &in = StreamFactory::getInstance().getInputStream(url);

        // Seek to the beginning of the input stream.
        in.clear();
        in.seekg(ios::beg);

        // The "current" container contains the data to be used for visualization...
        Container current;
        // ... whereas the "successor" container contains the data that follows the current one.
        Container successor;

        // Delta between to containers.
        uint32_t deltaInMicroseconds = 0;

        // This variable tracks the state.
        bool firstPass = true;

        // Rudimentary clock.
        Clock clock;

        uint32_t frameCounter = 0;
        while ( (getModuleState() == ModuleState::RUNNING) && (in.good()) ) {
            if (firstPass) {
                in >> current;
                firstPass = false;
            }
            else {
                current = successor;
            }
            in >> successor;

            deltaInMicroseconds = 0;
            if ( (current.getDataType() != Container::UNDEFINEDDATA) &&
                    (successor.getDataType() != Container::UNDEFINEDDATA) ) {
                TimeStamp delta = successor.getReceivedTimeStamp() - current.getReceivedTimeStamp();
                if (delta.toMicroseconds() > 0) {
                    deltaInMicroseconds = delta.toMicroseconds();
                }
            }

            if (current.getDataType() != Container::UNDEFINEDDATA) {
                m_renderer->process(current);
            }

            while (deltaInMicroseconds != 0) {
                // Every 40ms render a frame.
                if ( ( (clock.getMillisecondsSinceStart() % 40) == 0) && (clock.getFractionalMicroseconds() == 0) ) {
                    Lock l(m_conditionRenderer);
                    m_doRendering = true;
                    m_conditionRenderer.wakeAll();
                    frameCounter++;

                    if ((frameCounter%10) == 0) {
                        cerr << "(rec2video): " << frameCounter << " frames rendered." << endl;
                    }
                }

                if ( ( (clock.getMillisecondsSinceStart() % 1) == 0) && (clock.getFractionalMicroseconds() == 0) ) {
                    // TODO: Call simulation.
                }
                clock.incrementByOneMicrosecond();
                deltaInMicroseconds--;
            }
        }
        Lock l(m_conditionRenderer);
        m_doRendering = true;
        m_conditionRenderer.wakeAll();

        cerr << "(rec2video): Rendering done. Try to create a movie by using 'mencoder \"mf://*.png\" -mf fps=25:w=640:h=480 -ovc lavc -lavcopts vcodec=mpeg4:vbitrate=20000000 -nosound -o outfile.avi' for example." << endl;

        // This exit(0) is necessary since glutMainLoop() runs in another thread.
        exit(0);

        return ModuleState::OKAY;
    }
} // rec2video

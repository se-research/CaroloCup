/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "core/macros.h"
#include "core/base/KeyValueDataStore.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/data/recorder/RecorderCommand.h"
#include "core/io/StreamFactory.h"
#include "core/io/URL.h"

#include "Recorder.h"

namespace recorder {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::io;

    Recorder::Recorder(const int32_t &argc, char **argv) :
            ConferenceClientModule(argc, argv, "Recorder"),
            m_fifo() {}

    Recorder::~Recorder() {}

    void Recorder::setUp() {}

    void Recorder::tearDown() {}

    void Recorder::wait() {
        AbstractModule::wait();
    }

    void Recorder::recordQueueEntries(core::base::FIFOQueue &fifo, ostream &out) {
        if (!fifo.isEmpty()) {
            uint32_t numberOfEntries = fifo.getSize();
            for (uint32_t i = 0; i < numberOfEntries; i++) {
                Container c = fifo.leave();
                // Filter undefined data as well as recorder commands.
                if ( (c.getDataType() != Container::UNDEFINEDDATA) &&
                        (c.getDataType() != Container::RECORDER_COMMAND) ) {
                    out << c;
                }
            }
            out.flush();
        }
    }

    ModuleState::MODULE_EXITCODE Recorder::body() {
        // Check if the recorder is remotely controlled.
        bool remoteControl = (getKeyValueConfiguration().getValue<bool>("recorder.remoteControl") != 0);

        // Get output file.
        URL url(getKeyValueConfiguration().getValue<string>("recorder.output"));
        ostream &out = StreamFactory::getInstance().getOutputStream(url);
        // Add FIFOQueue to record all data.
        addDataStoreFor(m_fifo);

        // Get key/value-datastore for controlling the recorder.
        KeyValueDataStore &kvds = getKeyValueDataStore();

        // If remote control is disabled, simply start recording immediately.
        bool recording = (!remoteControl);
        while (getModuleState() == ModuleState::RUNNING) {
            // Recording queued entries.
            if (recording) {
                if (!m_fifo.isEmpty()) {
                    recordQueueEntries(m_fifo, out);
                } else {
                    Thread::usleep(500);
                }
            }

            // Check for remote control.
            if (remoteControl) {
                Container container = kvds.get(Container::RECORDER_COMMAND);
                if (container.getDataType() == Container::RECORDER_COMMAND) {
                    core::data::recorder::RecorderCommand rc;
                    rc = container.getData<core::data::recorder::RecorderCommand>();

                    recording = (rc.getCommand() == core::data::recorder::RecorderCommand::RECORD);
                }

                // Discard existing entries.
                if (!recording) {
                    m_fifo.clear();
                }
            }
        }

        // Record remaining entries.
        cout << "Clearing queue... ";
        recordQueueEntries(m_fifo, out);
        out.flush();
        cout << "done." << endl;

        return ModuleState::OKAY;
    }

} // recorder

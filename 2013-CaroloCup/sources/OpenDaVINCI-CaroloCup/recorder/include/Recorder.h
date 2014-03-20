/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef RECORDER_H_
#define RECORDER_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"

namespace recorder {

    using namespace std;

    /**
     * This class can be used to record data distributed
     * using a conference.
     */
    class Recorder : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Recorder(const Recorder &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Recorder& operator=(const Recorder &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Recorder(const int32_t &argc, char **argv);

            virtual ~Recorder();

            virtual void wait();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            core::base::FIFOQueue m_fifo;

            virtual void setUp();

            virtual void tearDown();

            /**
             * This method records data from the given FIFOQueue.
             *
             * @param fifo FIFOQueue to be used for recording data.
             * @param out Output stream to be used for writing data.
             */
            void recordQueueEntries(core::base::FIFOQueue &fifo, ostream &out);
    };

} // recorder

#endif /*RECORDER_H_*/

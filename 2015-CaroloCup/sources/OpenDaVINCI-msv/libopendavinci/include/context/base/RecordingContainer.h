/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_RECORDINGCONTAINER_H_
#define CONTEXT_BASE_RECORDINGCONTAINER_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "context/base/SystemReportingComponent.h"
#include "tools/recorder/Recorder.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class can be used to record all sent Containers.
         */
        class OPENDAVINCI_API RecordingContainer : public SystemReportingComponent {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
				RecordingContainer(const RecordingContainer&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
				RecordingContainer& operator=(const RecordingContainer&);

            public:
                /**
                 * Constructor.
                 *
                 * @param freq Frequency to be used.
                 * @param url URL of the file to be used for storing containers.
                 */
				RecordingContainer(const float &freq, const string &urlFileName, const uint32_t &memorySegmentSize, const uint32_t &numberOfSegments);

				virtual ~RecordingContainer();

                virtual void setup();

                virtual void tearDown();

                virtual void report(const core::wrapper::Time &t);

            private:
                virtual float getFrequency() const;

            private:
                float m_freq;
                string m_urlFileName;
                uint32_t m_memorySegmentSize;
                uint32_t m_numberOfSegments;
                tools::recorder::Recorder *m_recorder;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_RECORDINGCONTAINER_H_*/

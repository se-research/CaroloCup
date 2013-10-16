/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_RECORDINGCONTAINER_H_
#define CONTEXT_BASE_RECORDINGCONTAINER_H_

#include <fstream>
#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "context/base/SystemReportingComponent.h"

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
                 * @param fileName Name of the file to be used for storing containers.
                 */
				RecordingContainer(const float &freq, const string &fileName);

				virtual ~RecordingContainer();

                virtual void setup();

                virtual void tearDown();

                virtual void report(const core::wrapper::Time &t);

            private:
                float m_freq;
                string m_fileName;
                fstream m_outputFile;

                virtual float getFrequency() const;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_RECORDINGCONTAINER_H_*/

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/data/Container.h"
#include "context/base/RecordingContainer.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::data;
        using namespace tools::recorder;

		RecordingContainer::RecordingContainer(const float &freq, const string &urlFileName, const uint32_t &memorySegmentSize, const uint32_t &numberOfSegments) :
			m_freq(freq),
            m_urlFileName(urlFileName),
            m_memorySegmentSize(memorySegmentSize),
            m_numberOfSegments(numberOfSegments),
            m_recorder(NULL) {
        }

		RecordingContainer::~RecordingContainer() {
            OPENDAVINCI_CORE_DELETE_POINTER(m_recorder);
        }

		void RecordingContainer::setup() {
			if (m_urlFileName != "") {
                // We can use the sychronous recorder as we are running in a deterministic simulation anyways.
                const bool THREADING = false;
                m_recorder = new Recorder(m_urlFileName, m_memorySegmentSize, m_numberOfSegments, THREADING);
			}
		}

        void RecordingContainer::tearDown() {}

		void RecordingContainer::report(const core::wrapper::Time &/*t*/) {
			if (m_recorder != NULL) {
	            const uint32_t SIZE = getFIFO().getSize();
	            for (uint32_t i = 0; i < SIZE; i++) {
	                Container c = getFIFO().leave();
	                cerr << "(RecordingContainer) Storing '" << c.toString() << "'." << endl;
                    m_recorder->store(c);
	            }
			}
			getFIFO().clear();
		}

		float RecordingContainer::getFrequency() const {
			return m_freq;
		}

    }
} // context::base

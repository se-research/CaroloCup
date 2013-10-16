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

		RecordingContainer::RecordingContainer(const float &freq, const string &fileName) :
			m_freq(freq),
			m_fileName(fileName),
			m_outputFile() {}

		RecordingContainer::~RecordingContainer() {}

		void RecordingContainer::setup() {
			if (m_fileName != "") {
				m_outputFile.open(m_fileName.c_str(), ios::out | ios::binary | ios::trunc);
			}
		}

        void RecordingContainer::tearDown() {
			if (m_outputFile.is_open()) {
				m_outputFile.flush();
				m_outputFile.close();
			}
        }

		void RecordingContainer::report(const core::wrapper::Time &/*t*/) {
			if (m_outputFile.is_open()) {
	            const uint32_t SIZE = getFIFO().getSize();
	            for (uint32_t i = 0; i < SIZE; i++) {
	                Container c = getFIFO().leave();
	                cerr << "(RecordingContainer) Storing '" << c.toString() << "'." << endl;
	                m_outputFile << c;
	            }

	            m_outputFile.flush();
			}
			getFIFO().clear();
		}

		float RecordingContainer::getFrequency() const {
			return m_freq;
		}

    }
} // context::base

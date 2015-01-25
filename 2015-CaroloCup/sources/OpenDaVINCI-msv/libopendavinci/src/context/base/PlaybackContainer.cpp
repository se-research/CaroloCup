/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/data/Container.h"
#include "context/base/PlaybackContainer.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::data;
        using namespace tools::player;

		PlaybackContainer::PlaybackContainer(const float &freq, const string &urlFileName, const uint32_t &memorySegmentSize, const uint32_t &numberOfSegments) :
			m_freq(freq),
            m_urlFileName(urlFileName),
            m_memorySegmentSize(memorySegmentSize),
            m_numberOfSegments(numberOfSegments),
            m_player(NULL) {
        }

		PlaybackContainer::~PlaybackContainer() {
            OPENDAVINCI_CORE_DELETE_POINTER(m_player);
        }

		void PlaybackContainer::setup() {
			if (m_urlFileName != "") {
                // We can use the sychronous player as we are running in a deterministic simulation anyways.
                const bool THREADING = false;
                const bool AUTO_REWIND = false;
                m_player = new Player(m_urlFileName, AUTO_REWIND, m_memorySegmentSize, m_numberOfSegments, THREADING);
			}
		}

        void PlaybackContainer::tearDown() {}

        void PlaybackContainer::step(const core::wrapper::Time &/*t*/, SendContainerToSystemsUnderTest &sender) {
			if (m_player != NULL) {
                // TODO: Check if the delta between two captures frames matches the t and replay more than one frame potentially.
			    Container c;
                if (m_player->hasMoreData()) {
    				c = m_player->getNextContainerToBeSent();
                    sender.sendToSystemsUnderTest(c);
                }
			}

            // Discard all received containers.
			getFIFO().clear();
		}

		float PlaybackContainer::getFrequency() const {
			return m_freq;
		}

    }
} // context::base

/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_PLAYBACKCONTAINER_H_
#define CONTEXT_BASE_PLAYBACKCONTAINER_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "context/base/SystemFeedbackComponent.h"
#include "tools/player/Player.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class can be used to playback all recorded Containers.
         */
        class OPENDAVINCI_API PlaybackContainer : public SystemFeedbackComponent {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
				PlaybackContainer(const PlaybackContainer&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
				PlaybackContainer& operator=(const PlaybackContainer&);

            public:
                /**
                 * Constructor.
                 *
                 * @param freq Frequency to be used.
                 * @param url URL of the file to be used for storing containers.
                 */
				PlaybackContainer(const float &freq, const string &urlFileName, const uint32_t &memorySegmentSize, const uint32_t &numberOfSegments);

				virtual ~PlaybackContainer();

                virtual void setup();

                virtual void tearDown();

                virtual void step(const core::wrapper::Time &t, SendContainerToSystemsUnderTest &sender);

            private:
                virtual float getFrequency() const;

            private:
                float m_freq;
                string m_urlFileName;
                uint32_t m_memorySegmentSize;
                uint32_t m_numberOfSegments;
                tools::player::Player *m_player;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_PLAYBACKCONTAINER_H_*/

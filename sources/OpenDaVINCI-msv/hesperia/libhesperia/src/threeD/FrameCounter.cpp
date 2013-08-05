#include "hesperia/threeD/FrameCounter.h"

namespace hesperia {
    namespace threeD {

        using namespace core::data;

        FrameCounter::FrameCounter() :
            m_frameCounter(0),
            m_lastFrame(),
            m_fps(0)
        {}

        FrameCounter::~FrameCounter()
        {}

        void FrameCounter::reset() {
            m_frameCounter = 0;
            m_lastFrame = TimeStamp();
            m_fps = 0.0;
        }

        void FrameCounter::update() {
            TimeStamp current;
            TimeStamp duration = current - m_lastFrame;
            m_frameCounter++;

            if ( duration.toMicroseconds() > 1000*1000) {
                m_fps = m_frameCounter / duration.getSecond();
                m_frameCounter = 0;
                m_lastFrame = TimeStamp();
            }
        }


        double FrameCounter::getFPS() {
            return m_fps;
        }
    }
}

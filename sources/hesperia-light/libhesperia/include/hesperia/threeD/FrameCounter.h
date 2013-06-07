/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_THREED_FRAMECOUNTER_H_
#define HESPERIA_THREED_FRAMECOUNTER_H_

#include "core/data/TimeStamp.h"

namespace hesperia {
    namespace threeD {

        class FrameCounter
        {
            public:
                FrameCounter();
                ~FrameCounter();

                void reset();

                void update();

                double getFPS();

            protected:
                uint32_t m_frameCounter;
                core::data::TimeStamp m_lastFrame;
                double m_fps;
        };
    }
}
#endif // HESPERIA_THREED_CAMERA_H_

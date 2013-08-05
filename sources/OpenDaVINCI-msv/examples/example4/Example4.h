/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef EXAMPLE4_H_
#define EXAMPLE4_H_

#include <opencv/highgui.h>

#include "core/base/ConferenceClientModule.h"

namespace linearkalmanfilter {

    using namespace std;

    class LinearKalmanFilter : public core::base::ConferenceClientModule {
        private:
            enum {
                HEIGHT = 500,
                WIDTH = 500
            };

        public:
            LinearKalmanFilter(const int32_t &argc, char **argv);

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            LinearKalmanFilter(const LinearKalmanFilter &/*obj*/) :
                    core::base::ConferenceClientModule(0, NULL, "LinearKalmanFilter") {};

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            LinearKalmanFilter& operator=(const LinearKalmanFilter &/*obj*/) {
                return (*this);
            };

            void setUp();
            void tearDown();

            /**
             * This method draws a cross around the given point.
             *
             * @param img Image on which the cross has to be drawn.
             * @param point Point to be drawn.
             * @param d Distance from the center of the cross to the corner.
             * @param r R part of the color.
             * @param g G part of the color.
             * @param b B part of the color.
             */
            void drawCross(IplImage *img, const CvPoint &point, const double d, const uint32_t r, const uint32_t g, const uint32_t b);
    };
}

#endif /*EXAMPLE4_H_*/

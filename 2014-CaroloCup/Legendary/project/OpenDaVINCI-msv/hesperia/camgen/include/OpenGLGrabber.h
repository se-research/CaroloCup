/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CAMGEN_OPENGLGRABBER_H_
#define CAMGEN_OPENGLGRABBER_H_

#include <opencv/cv.h>
#include <opencv/highgui.h>

#include "core/SharedPointer.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/wrapper/Image.h"
#include "core/wrapper/SharedMemory.h"

#include "hesperia/data/environment/EgoState.h"
#include "hesperia/io/camera/ImageGrabber.h"
#include "hesperia/threeD/TransformGroup.h"

namespace camgen {

    using namespace std;

    /**
     * This class implements a grabber providing images from
     * a given OpenGL scene.
     */
    class OpenGLGrabber : public hesperia::io::camera::ImageGrabber {
        public:
            enum RENDERING {
                WORLD,
                INTRINSIC_PATTERN,
                EXTRINSIC_PATTERN
            };

        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            OpenGLGrabber(const OpenGLGrabber &);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            OpenGLGrabber& operator=(const OpenGLGrabber &);

        public:
            /**
             * Constructor.
             *
             * @param kvc KeyValueConfiguration.
             * @param imageGrabberID Identifier for this image grabber.
             * @param imageGrabberCalibration Calibration information for this grabber.
             * @param egoState Current ego state.
             */
            OpenGLGrabber(const core::base::KeyValueConfiguration &kvc,
                          const hesperia::data::camera::ImageGrabberID &imageGrabberID,
                          const hesperia::data::camera::ImageGrabberCalibration &imageGrabberCalibration,
                          hesperia::data::environment::EgoState &egoState);

            virtual ~OpenGLGrabber();

            virtual void delay();

            virtual core::SharedPointer<core::wrapper::Image> getNextImage();

            enum RENDERING m_render;
        private:
            core::base::KeyValueConfiguration m_kvc;
            core::SharedPointer<core::wrapper::Image> m_image;
            core::SharedPointer<core::wrapper::SharedMemory> m_sharedMemory;
            core::SharedPointer<hesperia::threeD::TransformGroup> m_root;
            core::SharedPointer<hesperia::threeD::TransformGroup> m_extrinsicCalibrationRoot;
            core::SharedPointer<hesperia::threeD::TransformGroup> m_intrinsicCalibrationRoot;
            hesperia::data::environment::EgoState &m_egoState;

            /**
             * This method renders the real word.
             */
            void renderNextImageFromRealWord();

            /**
             * This method renders the root for intrinsic camera calibration.
             */
            void renderNextImageFromIntrinsicCalibrationBody();

            /**
             * This method renders the root for extrinsic camera calibration.
             */
            void renderNextImageFromExtrinsicCalibrationBody();
    };

} // camgen

#endif /*CAMGEN_OPENGLGRABBER_H_*/
